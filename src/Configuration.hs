module Configuration
    ( feedConfiguration
    , siteWriterOptions
    , hakyllConfiguration
    , hyphLang
    ) where

import Hakyll
import Text.Pandoc.Options
import Text.Hyphenation            (english_GB, Hyphenator(..))
import Text.Hyphenation.Exception  (addException)

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
                   ++ " _site/ 'umibox:~http/main/'"
    }

hyphLang = lang { hyphenatorLeftMin = 1
                , hyphenatorRightMin = 1
                , hyphenatorExceptions = exceptions
                }
  where
    lang = english_GB
    exceptions = foldr addException (hyphenatorExceptions lang)
        [ "si-mo-nas"
        , "lith-u-a-ni-a"
        ]
