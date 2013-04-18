{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( entryItemTpl
    , applyTemplate
    , applyTemplateList
    ) where

import Data.Monoid                  (mappend)
import Hakyll                       (Context(..), Item, Compiler, itemSetBody, missingField, itemBody)
import Control.Applicative          ((<$>), liftA2, (<*>))
import Data.List                    (intersperse)

import Text.Blaze
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String

type Template m a = (String -> Item a -> m a) -> Item a -> m a

html <!> attr = (liftA2 (!)) html attr
html <<!>> attr = (return html) <!> (return attr)
html <<!> attr = (return html) <!> attr


applyTemplate :: Template Compiler String
              -> Context String -> Item String -> Compiler (Item String)
applyTemplate tpl ctx item =
    tpl ctx' item >>= \body ->
        return $ itemSetBody body item
  where
    ctx' :: String -> Item String -> Compiler String
    ctx' key item = unContext (ctx `mappend` missingField) key item


applyTemplateListWith :: String
                      -> Template Compiler String -> Context String
                      -> [Item String] -> Compiler String
applyTemplateListWith delimiter tpl ctx items =
    mapM (applyTemplate tpl ctx) items >>= \items' ->
       return $ concat $ intersperse delimiter $ map itemBody items'
applyTemplateList = applyTemplateListWith ""


entryItemTpl :: Template Compiler String
entryItemTpl context item = do
    date <- toHtml <$> context "date" item
    title <- toHtml <$> context "title" item
    url <- href <$> toValue <$> context "url" item

    return $ renderHtml $
        li $ do
            time ! class_ "blogentry-date" $ date
            a ! url $ title


