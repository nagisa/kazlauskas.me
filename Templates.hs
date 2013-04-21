{-# LANGUAGE OverloadedStrings #-}
module Templates (indexTpl, module Tpls) where

import Templates.Internal as Tpls
import Templates.Base     as Tpls
import Templates.Entry    as Tpls

import Hakyll                                (MonadMetadata, itemBody)
import Text.Blaze                            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String       (renderHtml)


indexTpl :: MonadMetadata m => Template m String
indexTpl context item = do
    entries <- entriesTpl context item
    return $ renderHtml $
        H.article ! A.id "index" $ do
            H.preEscapedToHtml $ itemBody item
            H.section ! A.id "recent-entries" $ do
                H.h1 ! A.id "rbp" $ toHtml "Recent blog entries"
                toHtml "(a "
                H.a ! A.href "/entries.html" $ toHtml "complete list"
                toHtml ")"
                H.preEscapedToHtml entries
