{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Monoid         (mappend, mconcat)
import           Hakyll
import           Text.Hyphenation    (hyphenate, english_GB, hyphenatorRightMin)
import           Text.Pandoc         (bottomUp, Pandoc)
import           Text.Pandoc.Definition
import           Data.List           (intercalate)


-- Returns a compiled list of item representations with links
entryListing :: Context a -> [Item a] -> Compiler String
entryListing ctx entry = do
    entryItemTpl <- loadBody "templates/entry-item.html"
    applyTemplateList entryItemTpl ctx entry


feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "Simonas Kazlauskas"
    , feedDescription = "kazlauskas.me blog entries"
    , feedAuthorName  = "Simonas Kazlauskas"
    , feedAuthorEmail = "web@kazlauskas.me"
    , feedRoot        = "http://kazlauskas.me"
    }


entryCtx tags = mconcat [ dateField "date" "%Y–%m–%d"
                        , tagsField "tags" tags
                        , defaultContext
                        ]

feedCtx = bodyField "description" `mappend` defaultContext

makePostList ctx = makeItem ""
                >>= loadAndApplyTemplate "templates/entries.html" ctx
                >>= loadAndApplyTemplate "templates/base.html" ctx
                >>= relativizeUrls

hyphenatePandoc :: Pandoc -> Pandoc
hyphenatePandoc = bottomUp (hyphInline :: Inline -> Inline)
                  where hypher = english_GB { hyphenatorRightMin = 2 }
                        hyphW = intercalate "\x00AD" . hyphenate hypher
                        hyphWs = unwords . map hyphW . words
                        hyphInline (Str str) = Str $ hyphWs str
                        hyphInline a = a

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    forM_ ["data/**", "images/**"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
------------------------------------------------------------------------------
    tags <- buildTags "entries/*" (fromCapture "tags/*.html")

    match "entries/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions
                                              defaultHakyllWriterOptions
                                              hyphenatePandoc
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/entry.html" (entryCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (entryCtx tags)
            >>= relativizeUrls

    -- Build a page and a separate rss feed for each tag
    tagsRules tags $ \tag pattern -> do
            let title = "Entries tagged ‘" ++ tag ++ "’"
            route idRoute
            compile $ do
                items <- loadAll pattern
                let itemsList = entryListing (entryCtx tags) $ recentFirst items
                let tagCtx = mconcat [ constField "title" title
                                     , field "entrylist" $ \_ -> itemsList
                                     , defaultContext
                                     ]
                makePostList tagCtx

            version "rss" $ do
                route $ setExtension "xml"
                compile $ do
                    entries <- loadAllSnapshots pattern "content"
                    renderAtom feedConf feedCtx (take 25 $ recentFirst entries)
------------------------------------------------------------------------------
    create ["entries.html"] $ do
        route idRoute
        compile $ do
            items <- loadAll "entries/*"
            let itemsList = entryListing (entryCtx tags) $ recentFirst items
            let entriesCtx = mconcat [ constField "title" "Entries"
                                     , field "entrylist" $ \_ -> itemsList
                                     , defaultContext
                                     ]
            makePostList entriesCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            entries <- loadAllSnapshots "entries/*" "content"
            renderAtom feedConf feedCtx (take 25 $ recentFirst entries)

    match "index.html" $ do
        route idRoute
        compile $ do
            entries <- loadAll "entries/*"
            let pList = entryListing (entryCtx tags) (take 5 $ recentFirst entries)
            let indexCtx = mconcat [ field "entries" $ \_ -> pList
                                   , defaultContext
                                   ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    match "pgp.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls
