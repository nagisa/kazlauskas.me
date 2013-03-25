{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( entryListing
    , makePostList
    , loadAllSorted
    , setItemIdVersion
    , setItemsIdVersions
    , loadAndTryApplyTemplates
    ) where

import Configuration
import Hakyll
import Control.Applicative    ((<|>))
import Data.Binary            (Binary)
import Data.Typeable          (Typeable)


entryListing :: Context a -> [Item a] -> Compiler String
entryListing ctx i = loadBody "templates/entry-item.html"
                        >>= (\tpl -> applyTemplateList tpl ctx i)


makePostList ctx = makeItem ""
                        >>= loadAndApplyTemplate "templates/entries.html" ctx
                        >>= loadAndApplyTemplate "templates/base.html" ctx
                        >>= relativizeUrls


loadAllSorted :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAllSorted p = loadAll p >>= recentFirst


setItemIdVersion :: Maybe String -> Item a -> Item a
setItemIdVersion version (Item identifier body) =
    Item (setVersion version identifier) body
-- And… a convenience function
setItemsIdVersions v = map (setItemIdVersion v)


loadAndTryApplyTemplates :: [Identifier]
                         -> Context a
                         -> Item a
                         -> Compiler (Item String)
loadAndTryApplyTemplates (t:ts) c i =
    loadAndApplyTemplate t c i <|> loadAndTryApplyTemplates ts c i
