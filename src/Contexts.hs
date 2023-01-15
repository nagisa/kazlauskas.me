{-# LANGUAGE OverloadedStrings #-}
module Contexts
    ( entryContext
    , feedContext
    , baseContext
    , entryListContext
    , entriesByYearContext
    ) where

import Control.Applicative ((<|>), (<$>), empty)
import Control.Monad       (forM, (<=<))
import Data.List           (groupBy, stripPrefix)
import Data.Maybe          (fromMaybe)
import Data.Monoid         (mconcat, (<>))
import Data.Time.Calendar  (toGregorian)
import Data.Time.Clock     (utctDay)
import Data.Time.Format    (defaultTimeLocale)
import Hakyll
import Text.Pandoc.Options

import Utils (splitFootnotes)
import Configuration

entryContext = mconcat
    [ footnotesField "footnotes"
    , bodyField' "body"
    , dateField "date" "%Y-%m-%d"
    , dateField "list-date" "%m-%d"
    , modificationTimeField "updated" "%Y-%m-%d"
    , prettyUrlField "url"
    , defaultContext
    ]


feedContext = mconcat
    [ bodyField "description"
    , modificationTimeField "updated" "%Y-%m-%dT%TZ"
    , prettyUrlField "url"
    , defaultContext
    ]

baseContext = mconcat
    [ constField "common-unicode-ranges" fontRangesCss
    , defaultContext
    , constField "copy" "CC BY 3.0"
    ]

entryListContext = listField "entries" entryContext

entriesByYearContext :: String -> String -> String -> Context String
                     -> Compiler [Item String] -> Context a
entriesByYearContext key yK aK aC entries = listField key yearGroup entries'
  where
    -- Produces Items that contain (Year, [Entries]) tuples.
    entries' :: Compiler [Item (Integer, [Item String])]
    entries' = mapM makeItem =<< groupPairs <$> (entries >>= makePairs)
    -- Extract publish date from each item
    makePairs :: [Item a] -> Compiler [(Integer, Item a)]
    makePairs = mapM $ \i -> do
        (y, m, d) <- fmap (toGregorian . utctDay)
                          (getItemUTC defaultTimeLocale $ itemIdentifier i)
        return (y, i)
    groupPairs :: [(Integer, Item a)] -> [(Integer, [Item a])]
    groupPairs = map extractPairs . groupBy (\(a, _) (b, _) -> a == b)
    extractPairs g = (fst $ head g, map snd g)
    yearGroup = field yK (return . show . fst . itemBody) <>
        listFieldWith aK aC (return . snd . itemBody)


maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key value = field key $ maybe empty return <=< value

--Custom fields---------------------------------------------------------------

-- tocField :: String -> Context a
-- tocField name = field name (const $ fmap itemBody comp)
--   where
--     comp = fmap (writePandocWith wopt . readPandocWith ropt) getResourceBody
--     ropt = defaultHakyllReaderOptions
--     wopt = defaultHakyllWriterOptions { writerTableOfContents = True
--                                       , writerStandalone = True
--                                       , writerTemplate = "$toc$"
--                                       }

bodyField' :: String -> Context String
bodyField' key = field key (return . fst . splitFootnotes . itemBody)

footnotesField :: String -> Context String
footnotesField key = maybeField key (return . snd . splitFootnotes . itemBody)

-- Not that I care, but cloudflare redirects all `.html` to pages without `.html`, which is… nasty
-- to say the least. The downside is that hakyll's built-in server no longer works? Yikes.
prettyUrlField :: String -> Context a
prettyUrlField key = mapContext (stripSuffix ".html") $ urlField key
  -- bah!
  where stripSuffix a b = fromMaybe b $ reverse <$> stripPrefix (reverse a) (reverse b)
