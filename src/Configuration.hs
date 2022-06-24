module Configuration
    ( feedConfiguration
    , siteWriterOptions
    , hakyllConfiguration
    , hyphLang
    , fontRanges
    , fontRangesCss
    ) where

import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Highlighting    (pygments)
import Text.Hyphenation            (english_GB, Hyphenator(..))
import Text.Hyphenation.Exception  (addException)
import Data.List                   (intercalate)

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
    { writerHTMLMathMethod = MathML
    , writerSectionDivs = True
    , writerHighlightStyle = Just pygments
    }

hakyllConfiguration = defaultConfiguration
    { inMemoryCache = True
    , storeDirectory = "/tmp/hakyll-kazlauskas.me/"
    , tmpDirectory = "/tmp/hakyll-kazlauskas.me/tmp/"
    , deployCommand = "rsync --recursive --links --times --omit-dir-times"
                   ++ "  --specials --verbose --compress --checksum --rsh=ssh "
                   ++ " _site/ 'unkobox:/var/www/kazlauskas.me/'"
    }

hyphLang = lang { hyphenatorExceptions = exceptions }
  where
    lang = english_GB
    exceptions = foldr addException (hyphenatorExceptions lang)
        [ "si-mo-nas"
        , "lith-u-a-nia"
        , "dig-it-al-ocean" -- O-cean is not hyphenated as it does not satisfy
                            -- hyphenatorLeftMin
        ]

-- Obtained with glyphs.py
fontRangeList = [ "0-7f"
             , "a9"
             , "ad"
             , "ae"
             , "b5"
             , "b7"
             , "bd"
             , "d7"
             , "e9"
             , "ef"
             , "14d"
             , "3b3"
             , "2013"
             , "2014"
             , "2019"
             , "201c"
             , "201d"
             , "2026"
             , "2032"
             , "20ac"
             , "2190"
             , "21a9"
             , "2212"
             , "2227"
             , "2248"
             , "2261"
             , "226b"
             , "fe0e"]
fontRanges :: String
fontRanges = intercalate "," fontRangeList
fontRangesCss :: String
fontRangesCss = intercalate "," $ map ("U+" ++) fontRangeList
