{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative              ((<$>))
import Data.Monoid                      (mappend)
import Hakyll
import Data.List                        (intercalate)
import qualified GHC.IO.Encoding
import qualified System.IO

import Compilers
import Configuration
import Contexts
import Utils

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  hakyllWith hakyllConfiguration $ do
    match "templates/*" $ compile templateCompiler

    match ("images/**" .||. "data/**"
                     .&&. complement (fromRegex "(js|css|ttf)$")) $ do
      route   idRoute
      compile copyFileCompiler

    match "data/**.css" $ do
      route   idRoute
      compile compressCssCompiler

    match "data/**.js" $ do
      route   idRoute
      compile jsCompiler

    match "data/**.ttf" $ do
      version "common-ttf" $ do
        route   $ setExtension "c.ttf"
        compile $ hbSubsetCompiler ["--unicodes", fontRanges]
      version "all-ttf" $ do
        route   $ setExtension "a.ttf"
        compile $ hbSubsetCompiler ["--unicodes=*", "--unicodes-", fontRanges]
      version "common-woff2" $ do
          route   $ setExtension "c.woff2"
          compile $ subsetWoff2Compiler "common-ttf"
      version "all-woff2" $ do
          route   $ setExtension "a.woff2"
          compile $ subsetWoff2Compiler "all-ttf"

    match "_redirects" $ do
      route   $ constRoute "_redirects"
      compile $ copyFileCompiler

    match "_headers" $ do
      route   $ constRoute "_headers"
      compile $ copyFileCompiler

    match "entries/**.mkd" $ do
      route   $ setExtension "html"
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
      route   $ idRoute
      compile $ setItemsIdVersions Nothing . take 25
                <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                >>= renderAtom feedConfiguration feedContext . take 10
