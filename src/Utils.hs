{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( loadAllSorted
    , setItemIdVersion
    , setItemsIdVersions
    , splitFootnotes
    ) where

import Data.Binary                      (Binary)
import Data.Monoid                      (mappend)
import Data.Typeable                    (Typeable)
import Hakyll                    hiding (applyTemplateList, applyTemplate)
import Text.XML.Light
import Data.Maybe                       (fromJust, fromMaybe)
import Data.List                        (partition)

import Configuration


loadAllSorted :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAllSorted p = loadAll p >>= recentFirst


setItemIdVersion :: Maybe String -> Item a -> Item a
setItemIdVersion version (Item identifier body) =
    Item (setVersion version identifier) body
-- And… a convenience function
setItemsIdVersions v = map (setItemIdVersion v)


splitFootnotes :: String -> (String, Maybe String)
splitFootnotes = unparse . partition isFns . parse
  where
    classAttr = blank_name { qName = "class" }
    isFns = (==) "footnotes" . fromMaybe "" . findAttr classAttr
    parse = onlyElems . parseXML
    unparse ([] , cEls) = (unparseC cEls, Nothing)
    unparse (fEls, cEls) = (unparseC cEls, Just $ unparseF fEls)
    unparseC = concatMap showElement
    unparseF = concatMap (unparseC . filterChildrenName ((/=) "hr" . qName))
