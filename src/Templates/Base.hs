{-# LANGUAGE OverloadedStrings #-}
module Templates.Base (baseTpl) where

import Hakyll                                (Compiler, itemBody)
import Hakyll.Web.Template.Blaze
import Text.Blaze                            ((!), AttributeValue, Markup)
import Text.Blaze.Internal
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Map                    as M


menuList :: Markup
menuList = mapM_ (\(val, href) -> H.li $ H.a ! A.href href $ toHtml val)
    [ ("About me", "/index.html")
    , ("Entries", "/entries.html")
    , ("PGP", "/pgp.html")
    ]


viewport = "width=device-width, initial-scale=1, maximum-scale=1"


licences :: M.Map String H.Html
licences = M.fromList $ map conv l
  where
    conv (k, t, h) = (k, H.a ! A.href h ! A.title t $ toHtml k)
    l = [ ("CC BY 3.0", "Creative Commons Attribution 3.0 Unported"
          , "http://creativecommons.org/licenses/by/3.0/deed.en_GB")
        , ("WTFPL", "Do What the F*** You Want to Public License"
          , "http://www.wtfpl.net/about/")
        ]


main = customParent "main"
role = customAttribute "role"


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
                H.title $ toHtml $ "Simonas Kazlauskas about " ++ title
                H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                       ! A.href "/data/sandwich.4.css"
                H.link ! A.rel "alternate" ! A.type_ "application/atom+xml"
                       ! A.href "/feed.atom"
                H.script ! A.async "" ! A.type_ "text/javascript"
                         ! A.src "/data/site.js" $ toHtml ""

            H.body $ do
                H.header $ do
                    H.nav $ H.ul $ menuList
                    H.div ! A.id "title-box" $ do
                        H.div ! A.id "intro" $ toHtml "Simonas about"
                        H.div ! A.id "title" $ toHtml title
                main ! role "main" $ safeToHtml $ itemBody item
                H.footer $ do
                    H.div ! A.id "generator" $ do
                        toHtml "Proudly "
                        H.a ! A.href "http://jaspervdj.be/hakyll"
                            ! A.class_ "hakyll" $ toHtml "Hakyll"
                    H.span ! A.id "copy" $ licences M.! copy
