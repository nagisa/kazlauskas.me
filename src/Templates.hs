{-# LANGUAGE OverloadedStrings #-}
module Templates (indexTpl, module Tpls) where

import Templates.Base     as Tpls
import Templates.Entry    as Tpls

import Hakyll                                (Compiler, itemBody)
import Hakyll.Web.Template.Blaze
import Text.Blaze                            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


indexTpl :: Template Compiler String
indexTpl context item = do
    entries <- entriesTpl context item
    return $
        H.article ! A.id "index" $ do
            preEscapedString $ itemBody item
            H.section ! A.id "recent-entries" $ do
                H.h1 $ string "Recent blog entries"
                H.span ! A.id "rbp-sub" $
                    H.a ! A.href "/entries.html" $ string "full listing"
                entries
