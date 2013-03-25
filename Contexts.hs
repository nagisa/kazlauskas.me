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
tocField name = field name tocCompiler
  where
    tocCompiler _ = do
        i <- pandocCompilerWith defaultHakyllReaderOptions opt
        return $ itemBody i
    opt = defaultHakyllWriterOptions { writerTableOfContents = True
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
