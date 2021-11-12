module Compilers
    ( pandocCompilerWithHyph
    , pandocCompilerHyph
    , entryCompiler
    , jsCompiler
    ) where

import Configuration
import Utils                  (hyphText)
import Control.Applicative    ((<$>))
import Data.ByteString.Lazy   (ByteString)
import Hakyll
import Text.Jasmine           (minify)
import Text.Pandoc            (bottomUp, Pandoc)
import Text.Pandoc.Definition (Inline (Str))
import Text.Pandoc.Options
import Data.Text              as T


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
