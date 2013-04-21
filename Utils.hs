{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( entryListCompiler
    , loadAllSorted
    , setItemIdVersion
    , setItemsIdVersions
    ) where

import Configuration
import Control.Applicative    ((<|>))
import Data.Binary            (Binary)
import Data.Monoid            (mappend)
import Data.Typeable          (Typeable)
import Hakyll       hiding    (applyTemplateList, applyTemplate)

import Templates

entryListCompiler :: Context String -> [Item String] -> Compiler (Item String)
entryListCompiler ctx items = applyTemplateList entryItemTpl ctx items >>=
    \is -> makeItem "" >>= applyTemplate entriesTpl (ctx' is)
  where
    ctx' is = constField "entries" is `mappend` defaultContext


loadAllSorted :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAllSorted p = loadAll p >>= recentFirst


setItemIdVersion :: Maybe String -> Item a -> Item a
setItemIdVersion version (Item identifier body) =
    Item (setVersion version identifier) body
-- And… a convenience function
setItemsIdVersions v = map (setItemIdVersion v)
