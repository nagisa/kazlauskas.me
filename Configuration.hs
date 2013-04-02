module Configuration
    ( feedConfiguration
    , siteWriterOptions
    , hakyllConfiguration
    ) where

import Text.Pandoc.Options
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Simonas Kazlauskas"
    , feedDescription = "kazlauskas.me blog entries"
    , feedAuthorName  = "Simonas Kazlauskas"
    , feedAuthorEmail = "web@kazlauskas.me"
    , feedRoot        = "http://www.kazlauskas.me"
    }

siteWriterOptions :: WriterOptions
siteWriterOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathML Nothing
    , writerHtml5 = True
    , writerSectionDivs = True
    }

hakyllConfiguration = defaultConfiguration
    { inMemoryCache = True
    , storeDirectory = "/tmp/hakyll-kazlauskas.me/"
    , tmpDirectory = "/tmp/hakyll-kazlauskas.me/tmp/"
    , deployCommand = "rsync --recursive --links --times --omit-dir-times"
                   ++ "  --specials --verbose --compress --checksum --rsh=ssh "
                   ++ " _site/ 'umibox:/home/http/main/'"
    }

