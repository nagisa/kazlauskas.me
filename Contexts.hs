module Contexts
    ( entryContext
    , feedContext
    , indexContext
    , baseContext
    ) where

import           Control.Applicative    ((<|>), empty)
import qualified Data.Map as M
import           Data.Monoid            (mappend, mconcat)
import           Hakyll
import           Text.Pandoc.Options


entryContext tags = mconcat [ dateField "date" "%Y-%m-%d"
                            , tagsField "tags" tags
                            , modificationTimeField "updated" "%Y-%m-%d"
                            , notEmptyField $ tocField "toc"
                            , defaultContext
                            ]


feedContext = bodyField "description" `mappend` defaultContext


indexContext entries = constField "entries" entries `mappend` defaultContext


baseContext = copyField
              `mappend` copyURLField "copy-url" "copy" copyField
              `mappend` copyLongField "copy-long" "copy" copyField
              `mappend` defaultContext
  where
    copyField = defaultField "copy" "CC BY 3.0"


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


defaultField :: String -> String -> Context String
defaultField key defval = Context $ \k i ->
    if key == k then unContext metadataField k i <|> return defval else empty


copyURLField, copyLongField :: String -> String -> Context a -> Context a
copyURLField  = copyGenField snd
copyLongField = copyGenField fst
copyGenField func key from (Context a) = Context $ \k i ->
    if key == k
    then a from i >>= \lc -> return $ func $ licences M.! lc
    else empty
  where
    licences = M.fromList
        [ ("CC BY 3.0", ( "Creative Commons Attribution 3.0 Unported"
          , "http://creativecommons.org/licenses/by/3.0/deed.en_GB"))
        , ("WTFPL", ("Do What the F*** You Want to Public License"
          , "http://www.wtfpl.net/about/"))
        -- Add on demand
        ]
