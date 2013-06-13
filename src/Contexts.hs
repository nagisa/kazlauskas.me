{-# LANGUAGE OverloadedStrings #-}
module Contexts
    ( entryContext
    , feedContext
    , baseContext
    , entryListContext
    ) where

import Control.Applicative ((<|>), empty)
import Data.Monoid         (mappend, mconcat)
import Data.Maybe          (fromMaybe)
import Hakyll
import Text.Pandoc.Options

import Utils (splitFootnotes)

entryContext tags = mconcat [ splitFootnotesField "footnotes"
                            , splitBodyField "body"
                            , dateField "date" "%Y-%m-%d"
                            , modificationTimeField "updated" "%Y-%m-%d"
                            , failIfEmpty $ tocField "toc"
                            , tagsField "tags" tags
                            , defaultContext
                            ]


feedContext = mconcat [ bodyField "description"
                      , modificationTimeField "updated" "%Y-%m-%dT%TZ"
                      , defaultContext
                      ]

entryListContext tags entries = listField "entries" (entryContext tags) entries

baseContext = defaultField "copy" "CC BY 3.0" `mappend` defaultContext

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
