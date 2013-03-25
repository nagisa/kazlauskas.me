{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative    ((<$>), (<|>))
import Data.Monoid            (mappend)

import Hakyll
import Utils
import Configuration
import Compilers
import Contexts


main :: IO ()
main = hakyllWith hakyllConfiguration $ do
    tags <- buildTags "entries/*" (fromCapture "tags/*.html")

    match "templates/*" $ compile templateCompiler

    match ("data/**" .||. "images/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "entries/*" $ do
        let entryTemplates = [ "templates/entry.html"
                             , "templates/entry-no-toc.html"]
        route $ setExtension "html"
        compile $ entryCompiler
            >>= loadAndTryApplyTemplates entryTemplates (entryContext tags)
            >>= loadAndApplyTemplate "templates/base.html" (entryContext tags)
            >>= relativizeUrls

        version "for-atom" $ compile $ entryCompiler

    create ["feed.atom"] $ do
        route idRoute
        compile $ setItemsIdVersions Nothing . take 25
                  <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                  >>= renderAtom feedConfiguration feedContext

    create ["entries.html"] $ do
        route idRoute
        compile $ do
            entries <- loadAllSorted ("entries/*" .&&. hasNoVersion)
                       >>= entryListing (entryContext tags)
            makePostList $ constField "title" "Simonas' Entries" `mappend`
                           constField "entries" entries `mappend`
                           defaultContext

    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ do
            entries <- take 5
                       <$> loadAllSorted ("entries/*" .&&. hasNoVersion)
                       >>= entryListing (entryContext tags)

            pandocCompilerHyph
                >>= loadAndApplyTemplate "templates/index.html" (indexContext entries)
                >>= loadAndApplyTemplate "templates/base.html" defaultContext
                >>= relativizeUrls

    match "pages/pgp.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ pandocCompilerHyph
                  >>= loadAndApplyTemplate "templates/base.html" defaultContext
                  >>= relativizeUrls

    -- Build a page and a separate atom feed for each tag
    tagsRules tags $ \tag pattern -> do
        let title = "Entries about " ++ tag
        let fConf = feedConfiguration { feedTitle = t } where
            t = feedTitle feedConfiguration ++ " – " ++ title

        route idRoute
        compile $ loadAllSorted pattern
            >>= entryListing (entryContext tags)
            >>= makePostList . (\e -> constField "entries" e `mappend`
                                      constField "title" title `mappend`
                                      defaultContext)

        version "atom" $ do
            route $ setExtension "atom"
            compile $ do
                matches <- map (setVersion (Just "for-atom"))
                           <$> getMatches pattern
                setItemsIdVersions Nothing . take 25
                    <$> (mapM load matches >>= recentFirst) --loadAllSorted
                    >>= renderAtom fConf feedContext
