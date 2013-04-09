module Compilers
    ( pandocCompilerWithHyph
    , pandocCompilerHyph
    , entryCompiler
    , jsCompiler
    ) where

import Configuration
import Data.ByteString.Lazy   (ByteString)
import Data.List              (intercalate)
import Hakyll
import Text.Hyphenation       (hyphenate, english_GB, hyphenatorRightMin)
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
    hyphW = intercalate "\x00AD" . hyphenate english_GB
    hyphInline (Str str) = Str $ (unwords . map hyphW . words) str
    hyphInline a = a


pandocCompilerHyph :: Compiler (Item String)
pandocCompilerHyph =
    pandocCompilerWithHyph defaultHakyllReaderOptions siteWriterOptions


entryCompiler :: Compiler (Item String)
entryCompiler =
    pandocCompilerWithHyph defaultHakyllReaderOptions siteWriterOptions


jsCompiler :: Compiler (Item ByteString)
jsCompiler = getResourceLBS >>= withItemBody (return . minify)
