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
    tags <- buildTags "entries/*" (fromCapture "tags/*.html")
    let entryListCtx = entryListContext tags
        entryCtx     = entryContext tags

    match "templates/*" $ compile templateCompiler

    match ("images/**" .||. "data/**"
                       .&&. complement (fromRegex "(js|css)$")
                       .||. "data/tweets/**") $ do
        route   idRoute
        compile copyFileCompiler

    match ("data/**.css" .&&. (complement "data/tweets/**.css")) $ do
        route   idRoute
        compile compressCssCompiler

    match ("data/**.js" .&&. (complement "data/tweets/**.js")) $ do
        route   idRoute
        compile jsCompiler

    match ("redirects.conf") $ do
        route $ constRoute ".redirects.conf"
        compile copyFileCompiler

    match "entries/**.md" $ do
        route $ setExtension "html"
        compile $ entryCompiler
            >>= loadAndApplyTemplate "templates/entry.html" entryCtx
            >>= loadAndApplyTemplate "templates/base.html" baseContext
            -- >>= applyTemplate' baseTpl baseContext
            >>= relativizeUrls
        version "for-atom" $ compile entryCompiler

    create ["feed.atom"] $ do
        route idRoute
        compile $ setItemsIdVersions Nothing . take 25
                  <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                  >>= renderAtom feedConfiguration feedContext

    create ["entries.html"] $ do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/entry-list.html"
                (entryListCtx $ loadAllSorted ("entries/*" .&&. hasNoVersion))
            >>= loadAndApplyTemplate "templates/base.html"
                (constField "title" "his entries" `mappend` baseContext)
            >>= relativizeUrls

    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "")
                 `composeRoutes` setExtension "html"
        compile $
            pandocCompilerHyph
            >>= loadAndApplyTemplate "templates/index.html"
                (entryListCtx (take 5 <$>
                               loadAllSorted ("entries/*" .&&. hasNoVersion))
                `mappend` defaultContext)
            >>= loadAndApplyTemplate "templates/base.html" baseContext
            >>= relativizeUrls

    match "pages/pgp.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ pandocCompilerHyph
                  >>= loadAndApplyTemplate "templates/base.html" baseContext
                  >>= relativizeUrls

    -- Build a page and a separate atom feed for each tag
    tagsRules tags $ \tag pattern -> do
        let title = "Entries about " ++ tag
        let fConf = feedConfiguration {
            feedTitle = feedTitle feedConfiguration ++ " – " ++ title
        }

        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/entry-list.html"
                (entryListCtx (loadAllSorted pattern))
            >>= loadAndApplyTemplate "templates/base.html"
                (constField "title" title `mappend` baseContext)
            >>= relativizeUrls

        version "atom" $ do
            route $ setExtension "atom"
            compile $ do
                matches <- map (setVersion (Just "for-atom"))
                           <$> getMatches pattern
                setItemsIdVersions Nothing . take 25
                    <$> (mapM load matches >>= recentFirst) --loadAllSorted
                    >>= renderAtom fConf feedContext
