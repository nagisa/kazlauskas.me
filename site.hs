{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Monoid         (mappend, mconcat)
import           Hakyll


-- Returns a compiled list of item representations with links
postListing :: Context a -> [Item a] -> Compiler String
postListing ctx post = do
    postItemTpl <- loadBody "templates/post-item.html"
    applyTemplateList postItemTpl ctx post


feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "Simonas Kazlauskas"
    , feedDescription = "kazlauskas.me blog entries"
    , feedAuthorName  = "Simonas Kazlauskas"
    , feedAuthorEmail = "web@kazlauskas.me"
    , feedRoot        = "http://kazlauskas.me"
    }


postCtx tags = mconcat [ dateField "date" "%Y–%m–%d"
                       , tagsField "tags" tags
                       , defaultContext
                       ]

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    forM_ ["data/**", "images/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
------------------------------------------------------------------------------
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
            >>= relativizeUrls

    -- Build a page and a separate rss feed for each tag
    tagsRules tags $ \tag pattern -> do
            let title = "Posts with ‘" ++ tag ++ "’ tag"
            route idRoute
            compile $ do
                items <- recentFirst <$> loadAll pattern
                let itemsList = postListing (postCtx tags) items
                let tagCtx = mconcat [ constField "title" title
                                     , field "posts" $ \_ -> itemsList
                                     , defaultContext
                                     ]
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" tagCtx
                    >>= loadAndApplyTemplate "templates/base.html" tagCtx
                    >>= relativizeUrls

            version "rss" $ do
                let feedCtx = bodyField "description" `mappend` defaultContext
                route $ setExtension "xml"
                compile $ do
                    posts <- take 25 . recentFirst <$> loadAllSnapshots pattern "content"
                    renderAtom feedConf feedCtx posts
------------------------------------------------------------------------------
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            items <- recentFirst <$> loadAll "posts/*"
            let itemsList = postListing (postCtx tags) items
            let postsCtx = mconcat [ constField "title" "Posts"
                                   , field "posts" $ \_ -> itemsList
                                   , defaultContext
                                   ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/base.html" postsCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        let feedCtx = bodyField "description" `mappend` defaultContext
        route idRoute
        compile $ do
            posts <- take 25 . recentFirst <$>
                               loadAllSnapshots "posts/*" "content"
            renderAtom feedConf feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            items <- take 5 . recentFirst <$> loadAll "posts/*"
            let itemsList = postListing (postCtx tags) items
            let indexCtx = mconcat [ field "posts" $ \_ -> itemsList
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
