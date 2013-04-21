module Templates.Internal
    ( Template
    , applyTemplate
    , applyTemplateList
    , applyTemplateListWith
    , toHtml
    ) where

import           Hakyll           (Context(..), Item, Compiler, itemSetBody,
                                   missingField, itemBody)
import           Data.Monoid      (mappend)
import           Data.List        (intercalate)
import qualified Text.Blaze.Html5 as H

type Template m a = (String -> Item a -> m a) -> Item a -> m a

toHtml :: String -> H.Html
toHtml = H.toHtml

applyTemplate :: Template Compiler String
              -> Context String -> Item String -> Compiler (Item String)
applyTemplate tpl ctx item =
    tpl ctx' item >>= \body ->
        return $ itemSetBody body item
  where
    ctx' :: String -> Item String -> Compiler String
    ctx' = unContext (ctx `mappend` missingField)


applyTemplateListWith :: String
                      -> Template Compiler String -> Context String
                      -> [Item String] -> Compiler String
applyTemplateListWith delimiter tpl ctx items =
    mapM (applyTemplate tpl ctx) items >>= \items' ->
       return $ intercalate delimiter $ map itemBody items'

applyTemplateList = applyTemplateListWith ""
