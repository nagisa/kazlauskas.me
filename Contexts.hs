module Contexts
    ( entryContext
    , feedContext
    , indexContext
    , baseContext
    ) where

import           Control.Applicative    ((<|>), empty)
import           Data.Monoid            (mappend, mconcat)
import           Hakyll
import           Text.Pandoc.Options


entryContext tags = mconcat [ dateField "date" "%Y-%m-%d"
                            , tagsField "tags" tags
                            , modificationTimeField "updated" "%Y-%m-%d"
                            , tocField "toc"
                            , defaultContext
                            ]


feedContext = bodyField "description" `mappend` defaultContext


indexContext entries = constField "entries" entries `mappend` defaultContext


baseContext = defaultField "copy" "CC BY 3.0" `mappend` defaultContext

--Custom fields---------------------------------------------------------------

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
    if key == k then unContext metadataField k i <|> return defval else empty
