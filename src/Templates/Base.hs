{-# LANGUAGE OverloadedStrings #-}
module Templates.Base (baseTpl) where

import Hakyll                                (Compiler, itemBody)
import Hakyll.Web.Template.Blaze
import Text.Blaze                            ((!), AttributeValue, Markup)
import Text.Blaze.Internal                   (customParent, customAttribute)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Map                    as M


baseTpl :: Template Compiler String
baseTpl context item = do
    title <- context "title"
    copy <- context "copy"
    return $
        H.docTypeHtml ! A.lang "en-gb" $ do

            H.head $ do
                H.meta ! A.charset "utf-8"
                H.meta ! A.name "author" ! A.content "Simonas Kazlauskas"
                H.meta ! A.name "viewport" ! A.content viewport
                H.meta ! A.name "keywords" ! A.content keywords
                H.title $ string $ "Simonas Kazlauskas about " ++ title
                H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                       ! A.href "/data/sandwich.4.css"
                H.link ! A.rel "alternate" ! A.type_ "application/atom+xml"
                       ! A.href "/feed.atom"
                H.script ! A.async "" ! A.type_ "text/javascript"
                         ! A.src "/data/site.js" $ string ""

            H.body $ do
                H.header $ do
                    H.nav $ H.ul $ menuList
                    H.div ! A.id "title-box" $ do
                        H.div ! A.id "intro" $ string "Simonas about"
                        H.div ! A.id "title" $ string title
                main ! role "main" $ safeToHtml $ itemBody item
                H.footer $ do
                    H.div ! A.id "generator" $ do
                        string "Proudly "
                        H.a ! A.href "http://jaspervdj.be/hakyll"
                            ! A.class_ "hakyll" $ string "Hakyll"
                    H.span ! A.id "copy" $ licences M.! copy
  where
    main = customParent "main"
    role = customAttribute "role"
    viewport = "width=device-width, initial-scale=1, maximum-scale=1"
    keywords = "simonas, kazlauskas, simukis, nagisa, open source"


menuList :: H.Html
menuList = mapM_ (\(val, href) -> H.li $ H.a ! A.href href $ string val)
    [ ("About me", "/index.html")
    , ("Entries", "/entries.html")
    , ("PGP", "/pgp.html")
    ]


licences :: M.Map String H.Html
licences = M.fromList $ map conv l
  where
    conv (k, t, h) = (k, H.a ! A.href h ! A.title t $ string k)
    l = [ ("CC BY 3.0", "Creative Commons Attribution 3.0 Unported"
          , "http://creativecommons.org/licenses/by/3.0/deed.en_GB")
        , ("WTFPL", "Do What the F*** You Want to Public License"
          , "http://www.wtfpl.net/about/")
        ]

