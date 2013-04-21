{-# LANGUAGE OverloadedStrings #-}
module Templates.Entry
    ( entryTpl
    , entryItemTpl
    , entriesTpl
    ) where

import           Hakyll (MonadMetadata, itemBody)
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String

import           Templates.Internal

entryItemTpl, entryTpl, entriesTpl  :: MonadMetadata m => Template m String

entryItemTpl context item = do
    date <- context "date" item
    title <- context "title" item
    url <- context "url" item
    return $ renderHtml $
        H.li $ do
            H.time ! A.class_ "blogentry-date" $ toHtml date
            H.a ! A.href (toValue url) $ toHtml title

entryTpl context item = do
    date <- context "date" item
    update <- context "updated" item
    tags <- context "tags" item
    toc <- context "toc" item
    return $ renderHtml $
        H.article ! A.id "entry" $ do
            H.section ! A.id "metadata" $ do
                H.span ! A.class_ "metah" $ toHtml "Post date"
                H.time ! A.pubdate "" $ toHtml date
                H.span ! A.class_ "metah" $ toHtml "Last update"
                H.time $ toHtml update
                H.span ! A.class_ "metah" $ toHtml "Tags"
                H.preEscapedToHtml tags
                if toc == "" then mempty else do
                    H.span ! A.class_ "metah" $ toHtml "Table of contents"
                    H.preEscapedToHtml toc
            H.preEscapedToHtml $ itemBody item

entriesTpl context item = do
    entries <- context "entries" item
    return $ renderHtml $
        H.ul ! A.class_ "blogentries" $ H.preEscapedToHtml entries
