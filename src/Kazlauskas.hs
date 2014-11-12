{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative              ((<$>))
import Data.Monoid                      (mappend)
import Hakyll

import Compilers
import Configuration
import Contexts
import Utils


main :: IO ()
main = hakyllWith hakyllConfiguration $ do
    match "templates/*" $ compile templateCompiler

    match ("images/**" .||. "data/**"
                       .&&. complement (fromRegex "(js|css)$")
                       .||. "data/tweets/**") $ do
        route   idRoute
        compile copyFileCompiler

    match ("data/**.css" .&&. complement "data/tweets/**.css") $ do
        route   idRoute
        compile compressCssCompiler

    match ("data/**.js" .&&. complement "data/tweets/**.js") $ do
        route   idRoute
        compile jsCompiler

    match "redirects.conf" $ do
        route $ constRoute ".redirects.conf"
        compile copyFileCompiler

    match "entries/**.mkd" $ do
        route $ setExtension "html"
        compile $ fmap demoteHeaders <$> entryCompiler
            >>= loadAndApplyTemplate "templates/entry.html" entryContext
            >>= loadAndApplyTemplate "templates/base.html" baseContext
            >>= relativizeUrls
        version "for-atom" $ compile entryCompiler

    match "pages/*.html" $ do
        route   $ gsubRoute "pages/" (const "")
        compile $ let es = loadAllSorted ("entries/*" .&&. hasNoVersion) in
            getResourceBody
            >>= applyAsTemplate (entriesByYearContext "years" "year" "entries" entryContext es)
            >>= return . fmap hyphHTML
            >>= loadAndApplyTemplate "templates/base.html" baseContext
            >>= relativizeUrls

    create ["feed.atom"] $ do
        route idRoute
        compile $ setItemsIdVersions Nothing . take 25
                  <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                  >>= renderAtom feedConfiguration feedContext
