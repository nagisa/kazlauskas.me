{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative    ((<$>), (<|>))
import Data.Monoid            (mappend)

import Hakyll hiding (applyTemplate)
import Hakyll.Web.Template.Blaze
import Utils
import Configuration
import Compilers
import Contexts

import Templates


main :: IO ()
main = hakyllWith hakyllConfiguration $ do
    tags <- buildTags "entries/*" (fromCapture "tags/*.html")

    match ("images/**" .||. "data/**"
                       .&&. complement (fromRegex "(js|css)$")) $ do
        route   idRoute
        compile copyFileCompiler

    match "data/**.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "data/**.js" $ do
        route   idRoute
        compile jsCompiler

    match "entries/**.md" $ do
        route $ setExtension "html"
        compile $ entryCompiler
            >>= applyTemplate entryTpl (entryContext tags)
            >>= applyTemplate baseTpl baseContext
            >>= relativizeUrls
        version "for-atom" $ compile entryCompiler

    create ["feed.atom"] $ do
        route idRoute
        compile $ setItemsIdVersions Nothing . take 25
                  <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                  >>= renderAtom feedConfiguration feedContext

    create ["entries.html"] $ do
        route idRoute
        compile $
            loadAllSorted ("entries/*" .&&. hasNoVersion)
            >>= entryListCompiler (entryContext tags)
            >>= applyTemplate baseTpl
                (constField "title" "Simonas' Entries" `mappend` baseContext)
            >>= relativizeUrls



    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ do
            entries <- take 5
                       <$> loadAllSorted ("entries/*" .&&. hasNoVersion)
                       >>= entryListCompiler (entryContext tags)

            pandocCompilerHyph
                >>= applyTemplate indexTpl
                    (indexContext $ itemBody entries)
                >>= applyTemplate baseTpl baseContext
                >>= relativizeUrls

    match "pages/pgp.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ pandocCompilerHyph
                  >>= applyTemplate baseTpl baseContext
                  >>= relativizeUrls

    -- Build a page and a separate atom feed for each tag
    tagsRules tags $ \tag pattern -> do
        let title = "Entries about " ++ tag
        -- let fConf = feedConfiguration {
        --     feedTitle = feedTitle feedConfiguration ++ " – " ++ title
        -- }

        route idRoute
        compile $
            loadAllSorted pattern
            >>= entryListCompiler (entryContext tags)
            >>= applyTemplate baseTpl
                (constField "title" title `mappend` baseContext)
            >>= relativizeUrls

        -- version "atom" $ do
        --     route $ setExtension "atom"
        --     compile $ do
        --         matches <- map (setVersion (Just "for-atom"))
        --                    <$> getMatches pattern
        --         setItemsIdVersions Nothing . take 25
        --             <$> (mapM load matches >>= recentFirst) --loadAllSorted
        --             >>= renderAtom fConf feedContext
