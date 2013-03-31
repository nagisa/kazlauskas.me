module Contexts
    ( entryContext
    , feedContext
    , indexContext
    ) where

import Hakyll
import Data.Monoid            (mappend, mconcat)
import Text.Pandoc.Options


entryContext tags = mconcat [ dateField "date" "%Y-%m-%d"
                            , tagsField "tags" tags
                            , modificationTimeField "update" "%Y-%m-%d"
                            , notEmptyField $ tocField "toc"
                            , defaultContext
                            ]


feedContext = bodyField "description" `mappend` defaultContext


indexContext entries = constField "entries" entries `mappend` defaultContext


tocField :: String -> Context a
tocField name = field name (const $ fmap itemBody comp)
  where
    comp = fmap (writePandocWith wopt . readPandocWith ropt) getResourceBody
    ropt = defaultHakyllReaderOptions
    wopt = defaultHakyllWriterOptions { writerTableOfContents = True
                                      , writerStandalone = True
                                      , writerTemplate = "$toc$"
                                      }


notEmptyField :: Context a -> Context a
notEmptyField (Context a) = Context $ \k i -> do
    value <- a k i
    case value of
        []        -> fail $ "Empty field "++ k ++" in context for item "
                            ++ show (itemIdentifier i)
        _ -> return value
