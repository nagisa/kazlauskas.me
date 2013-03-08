{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Control.Applicative    ((<$>))
import Control.Monad          (mapM, liftM)
import Data.List              (intercalate, sortBy)
import Data.Monoid            (mappend, mconcat)
import Data.Ord               (comparing)
import Hakyll
import System.Locale          (defaultTimeLocale)
import Text.Hyphenation       (hyphenate, english_GB, hyphenatorRightMin)
import Text.Pandoc            (bottomUp, Pandoc)
import Text.Pandoc.Definition
import Text.Pandoc.Options


-- Returns a compiled list of item representations with links
entryListing :: Context a -> [Item a] -> Compiler String
entryListing ctx i = loadBody "templates/entry-item.html"
                        >>= (\tpl -> applyTemplateList tpl ctx i)

-- Builds a full featured page which lists entries
makePostList ctx = makeItem ""
                        >>= loadAndApplyTemplate "templates/entries.html" ctx
                        >>= loadAndApplyTemplate "templates/base.html" ctx
                        >>= relativizeUrls

entryCtx tags = mconcat [ dateField "date" "%Y-%m-%d"
                        , tagsField "tags" tags
                        , modificationTimeField "update" "%Y-%m-%d"
                        , defaultContext
                        ]

------------------------------------------------------------------------------
feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "Simonas Kazlauskas"
    , feedDescription = "kazlauskas.me blog entries"
    , feedAuthorName  = "Simonas Kazlauskas"
    , feedAuthorEmail = "web@kazlauskas.me"
    , feedRoot        = "http://www.kazlauskas.me"
    }

feedCtx = bodyField "description" `mappend` defaultContext

------------------------------------------------------------------------------
hyphenatePandoc :: Pandoc -> Pandoc
hyphenatePandoc = bottomUp (hyphInline :: Inline -> Inline)
                  where hypher = english_GB { hyphenatorRightMin = 2 }
                        hyphW = intercalate "\x00AD" . hyphenate hypher
                        hyphWs = unwords . map hyphW . words
                        hyphInline (Str str) = Str $ hyphWs str
                        hyphInline a = a

pandocCompilerWithHyph :: ReaderOptions
                       -> WriterOptions
                       -> Compiler (Item String)
pandocCompilerWithHyph ropt wopt =
    pandocCompilerWithTransform ropt wopt hyphenatePandoc

pandocCompilerHyph :: Compiler (Item String)
pandocCompilerHyph = pandocCompilerWithHyph defaultHakyllReaderOptions
                                            siteWriterOptions

-- Take care to understand that argument is a template for Pandoc. It should
-- contain at least $body$
entryCompiler :: Item String -> Compiler (Item String)
entryCompiler (Item _ tpl) =
    pandocCompilerWithHyph defaultHakyllReaderOptions (entryWriterOptions tpl)

siteWriterOptions :: WriterOptions
siteWriterOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathML Nothing
    , writerHtml5 = True
    , writerSectionDivs = True
    }

entryWriterOptions :: String -> WriterOptions
entryWriterOptions tpl = siteWriterOptions
    { writerTableOfContents = True
    , writerStandalone = True
    , writerTemplate = tpl
    }

------------------------------------------------------------------------------

-- loadAllSorted :: Pattern -> Compiler [Item a]
loadAllSorted p = loadAll p >>= recentFirst

------------------------------------------------------------------------------
-- Because we do not set route (and we do want it to point to regular version
-- anyway) we need to set identifier version for `entries (for-atom)` to
-- Nothing
setItemIdentifierVersion :: Maybe String -> Item a -> Item a
setItemIdentifierVersion version (Item identifier body) =
    Item (setVersion version identifier) body
-- And… a convenience function
setItemIdVersionList v = map (setItemIdentifierVersion v)

------------------------------------------------------------------------------
hakyllConf = defaultConfiguration
    { inMemoryCache = True
    , storeDirectory = "/tmp/hakyll-kazlauskas.me/"
    , tmpDirectory = "/tmp/hakyll-kazlauskas.me/tmp/"
    }


main :: IO ()
main = hakyllWith hakyllConf $ do
    tags <- buildTags "entries/*" (fromCapture "tags/*.html")

    match "templates/*" $ compile templateCompiler

    match ("data/**" .||. "images/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "entries/*" $ do
        route $ setExtension "html"
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/entry.html" (entryCtx tags)
            >>= entryCompiler
            >>= loadAndApplyTemplate "templates/base.html" (entryCtx tags)
            >>= relativizeUrls

        version "for-atom" $ do
            compile $ makeItem "$body$" >>= entryCompiler

    create ["feed.atom"] $ do
        route idRoute
        compile $ setItemIdVersionList Nothing . take 25
                  <$> loadAllSorted ("entries/*" .&&. hasVersion "for-atom")
                  >>= renderAtom feedConf feedCtx

    create ["entries.html"] $ do
        route idRoute
        compile $ do
            entries <- loadAllSorted ("entries/*" .&&. hasNoVersion)
                       >>= entryListing (entryCtx tags)
            makePostList $ constField "title" "Simonas' Entries" `mappend`
                           constField "entries" entries `mappend`
                           defaultContext

    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "")
                  `composeRoutes` setExtension "html"
        compile $ do
            entries <- take 5
                       <$> loadAllSorted ("entries/*" .&&. hasNoVersion)
                       >>= entryListing (entryCtx tags)
            let iCtx = constField "entries" entries `mappend` defaultContext

            pandocCompilerHyph
                >>= loadAndApplyTemplate "templates/index.html" iCtx
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
        route idRoute
        compile $ loadAllSorted pattern
            >>= entryListing (entryCtx tags)
            >>= makePostList . (\e -> constField "entries" e `mappend`
                                      constField "title" title `mappend`
                                      defaultContext)

        version "atom" $ do
            route $ setExtension "atom"
            compile $ do
                let fTitle = (feedTitle feedConf) ++ " – " ++ title
                matches <- map (setVersion (Just "for-atom"))
                           <$> getMatches pattern
                setItemIdVersionList Nothing . take 25
                    <$> (mapM load matches >>= recentFirst) --loadAllSorted
                    >>= renderAtom feedConf {feedTitle = fTitle} feedCtx
