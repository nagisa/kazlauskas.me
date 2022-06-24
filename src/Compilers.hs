module Compilers
    ( pandocCompilerWithHyph
    , pandocCompilerHyph
    , entryCompiler
    , jsCompiler
    , hbSubsetCompiler
    , subsetWoff2Compiler
    ) where

import Configuration
import Utils                  (hyphText)
import Control.Applicative    ((<$>))
import Data.ByteString.Lazy   (ByteString)
import Data.List              as L
import Data.Text              as T
import Hakyll
import Text.Jasmine           (minify)
import Text.Pandoc            (bottomUp, Pandoc)
import Text.Pandoc.Definition (Inline (Str))
import Text.Pandoc.Options


pandocCompilerWithHyph :: ReaderOptions
                       -> WriterOptions
                       -> Compiler (Item String)
pandocCompilerWithHyph ropt wopt =
    pandocCompilerWithTransform ropt wopt hyphenatePandoc
  where
    hyphenatePandoc = bottomUp (hyphInline :: Inline -> Inline)
    hyphInline (Str str) = Str . T.pack . hyphText . T.unpack $ str
    hyphInline a = a

pandocCompilerHyph :: Compiler (Item String)
pandocCompilerHyph =
    pandocCompilerWithHyph defaultHakyllReaderOptions siteWriterOptions

entryCompiler :: Compiler (Item String)
entryCompiler =
    pandocCompilerWithHyph defaultHakyllReaderOptions siteWriterOptions


jsCompiler :: Compiler (Item ByteString)
jsCompiler = getResourceLBS >>= withItemBody (return . minify)

hbSubsetCompiler :: [String] -> Compiler (Item ByteString)
hbSubsetCompiler options = do
    debugCompiler $ "Executing hb-subset with arguments: " ++ (L.intercalate " " options')
    getResourceLBS >>= withItemBody (unixFilterLBS "hb-subset" options')
  where options' = [ "-o", "-", "-" ] ++ options

subsetWoff2Compiler :: String -> Compiler (Item ByteString)
subsetWoff2Compiler version =
    setVersion (Just version)
    <$> getUnderlying
    >>= loadBody
    >>= unixFilterLBS "make-it-woff2" []
    >>= makeItem
