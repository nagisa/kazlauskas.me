{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative    ((<$>), (<|>))
import Data.Monoid            (mappend)

import Hakyll
import Utils
import Configuration
import Compilers
import Contexts
import qualified Templates as T


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
            >>= T.applyTemplate T.entryTpl (entryContext tags)
            >>= T.applyTemplate T.baseTpl baseContext
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
            >>= T.applyTemplate T.baseTpl
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
                >>= T.applyTemplate T.indexTpl
                    (indexContext $ itemBody entries)
                >>= T.applyTemplate T.baseTpl baseContext
                >>= relativizeUrls

    match "pages/pgp.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ pandocCompilerHyph
                  >>= T.applyTemplate T.baseTpl baseContext
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
            >>= T.applyTemplate T.baseTpl
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
