{-# LANGUAGE OverloadedStrings #-}
module Contexts
    ( entryContext
    , feedContext
    , baseContext
    , entryListContext
    , yearGroupedEntryListContext
    ) where

import Control.Applicative ((<|>), (<$>), empty)
import Data.Monoid         (mconcat, (<>))
import Control.Monad       (forM)
import Data.Maybe          (fromMaybe)
import Data.Time.Calendar  (toGregorian)
import Data.Time.Clock     (utctDay)
import Data.List           (groupBy)
import System.Locale       (defaultTimeLocale)
import Hakyll
import Text.Pandoc.Options

import Utils (splitFootnotes)

entryContext = mconcat [ splitFootnotesField "footnotes"
                       , splitBodyField "body"
                       , dateField "date" "%Y-%m-%d"
                       , dateField "list-date" "%m-%d"
                       , modificationTimeField "updated" "%Y-%m-%d"
                       , failIfEmpty $ tocField "toc"
                       , defaultContext
                       ]


feedContext = mconcat [ bodyField "description"
                      , modificationTimeField "updated" "%Y-%m-%dT%TZ"
                      , defaultContext
                      ]

entryListContext entries = listField "entries" entryContext entries


-- | TODO: refactor, perhaps?
--
-- I can't read this code that same evening I wrote it...
makeItemPairs :: MonadMetadata m => [Item a] -> m [(Item a, Integer)]
makeItemPairs = mapM $ \i -> do
    time <- getItemUTC defaultTimeLocale $ itemIdentifier i
    return $ (i, (\(a,b,c) -> a) $ toGregorian $ utctDay time)
groupItems :: [(Item a, Integer)] -> [(String, [Item a])]
groupItems = map (\g -> (show $ snd $ head g, map fst g))
             . groupBy (\(i1, y1) (i2, y2) -> y1 == y2)
oneYearGroup :: Context (String, [Item String])
oneYearGroup = field "year" (return . fst . itemBody) <> articlesField
  where
    articlesField :: Context (String, [Item String])
    articlesField = Context $ \k i -> if k == "entries"
        then return $ ListField entryContext (snd . itemBody $ i)
        else empty
yearGroupedEntryListContext :: Compiler [Item String] -> Context a
yearGroupedEntryListContext items = listField "years" oneYearGroup is
  where
    is :: Compiler [Item (String, [Item String])]
    is = fmap groupItems (items >>= makeItemPairs) >>= mapM makeItem


baseContext = defaultField "copy" "CC BY 3.0" <> defaultContext

--Custom fields---------------------------------------------------------------

-- | Needed for $if$ to evaluate as false when string returned by field is
--   empty
failIfEmpty :: Context a -> Context a
failIfEmpty ctx = Context $ \k i ->
    fmap StringField $ unContext ctx k i >>= go
  where
    go (StringField []) = empty
    go (StringField a) = return a
    go _ = fail "Only string fields are supported"

tocField :: String -> Context a
tocField name = field name (const $ fmap itemBody comp)
  where
    comp = fmap (writePandocWith wopt . readPandocWith ropt) getResourceBody
    ropt = defaultHakyllReaderOptions
    wopt = defaultHakyllWriterOptions { writerTableOfContents = True
                                      , writerStandalone = True
                                      , writerTemplate = "$toc$"
                                      }

defaultField :: String -> String -> Context String
defaultField key defval = Context $ \k i ->
    fmap StringField $ if key == k
        then (unContext metadataField k i >>= getString) <|> return defval
        else empty
  where
    getString (StringField a) = return a
    getString _ = fail "Only string fields are supported"

splitBodyField, splitFootnotesField :: String -> Context String
splitBodyField key = Context $ \k i ->
    fmap StringField $ if key == k
        then unContext (bodyField k) k i >>= go
        else empty
  where
    go (StringField a) = return . fst . splitFootnotes $ a
    go _ = fail "Only string fields are supported"
splitFootnotesField key = Context $ \k i ->
    fmap StringField $ if key == k
        then unContext (bodyField k) k i >>= go
        else empty
  where
    go (StringField a) = fromMaybe empty $ (fmap return (snd (splitFootnotes a)))
    go _ = fail "Only string fields are supported"
