{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Monoid         (mappend, mconcat)
import           Hakyll


main :: IO ()
main = hakyll $ do
    forM_ ["data/**", "images/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler
------------------------------------------------------------------------------
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
------------------------------------------------------------------------------
    match "pgp.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls
------------------------------------------------------------------------------
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
------------------------------------------------------------------------------
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
            >>= relativizeUrls
------------------------------------------------------------------------------
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = field "posts" (\_ -> postList tags "posts/*" recentFirst)
                             `mappend` constField "title" "Posts"
                             `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/base.html" archiveCtx
                >>= relativizeUrls
------------------------------------------------------------------------------
    tagsRules tags $ \tag pattern -> do
            let title = "Posts with ‘" ++ tag ++ "’ tag"
            route idRoute
            compile $ do
                list <- postList tags pattern recentFirst
                let tagCtx = constField "title" title `mappend`
                             constField "posts" list `mappend`
                             defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" tagCtx
                     >>= loadAndApplyTemplate "templates/base.html" tagCtx
                     >>= relativizeUrls

            version "rss" $ do
                route $ setExtension "xml"
                compile $ do
                    posts <- take 25 . recentFirst <$>
                             loadAllSnapshots "posts/*" "content"
                    renderAtom feedConf feedCtx posts
------------------------------------------------------------------------------
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- take 25 . recentFirst <$> loadAllSnapshots "posts/*" "content"
            renderAtom feedConf feedCtx posts
------------------------------------------------------------------------------
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> (postList tags "posts/*") $
                           take 5 . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
                >>= relativizeUrls
------------------------------------------------------------------------------
    match "templates/*" $ compile templateCompiler


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%Y–%m–%d"
    , tagsField "tags" tags
    , defaultContext
    ]


feedCtx :: Context String
feedCtx = defaultContext `mappend` bodyField "description"


postList :: Tags -> Pattern -> ([Item String] -> [Item String])
            -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/post-item.html"
    posts <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts


feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "Simonas Kazlauskas"
    , feedDescription = "kazlauskas.me blog entries"
    , feedAuthorName  = "Simonas Kazlauskas"
    , feedAuthorEmail = "web@kazlauskas.me"
    , feedRoot        = "http://kazlauskas.me"
    }
