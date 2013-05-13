{-# LANGUAGE OverloadedStrings #-}
module Templates.Entry
    ( entryTpl
    , entryItemTpl
    , entriesTpl
    ) where

import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid (mempty)
import           Data.List   (partition)
import           Hakyll (Compiler, itemBody, Item(..))
import           Hakyll.Web.Template.Blaze

import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String
import           Text.XML.Light


entryItemTpl, entryTpl, entriesTpl  :: Template Compiler String

entryItemTpl context item = do
    date <- context "date"
    title <- context "title"
    url <- context "url"
    return $
        H.li $ do
            H.time ! A.class_ "blogentry-date" $ string date
            H.a ! A.href (toValue url) $ string title

entryTpl context item = do
    let (body, fnotes) = splitFootnotes item
    date <- context "date"
    update <- context "updated"
    tags <- context "tags"
    toc <- context "toc"
    return $ do

        H.aside ! A.id "metadata" $ do
            metadataElement "Published" $ H.time ! A.pubdate "" $ string date
            if date == update then mempty else
                metadataElement "Updated" $ H.time $ string update
            metadataElement "Tags" $ safeToHtml tags

        H.article ! A.id "entry" $ do
            if toc == "" then mempty else
                H.aside ! A.id "toc" $ do
                    H.span ! A.id "toctitle" $ "Table of Contents"
                    safeToHtml toc
            safeToHtml body

        if fnotes == Nothing then mempty else H.aside ! A.id "footnotes" $ do
            H.h1 $ string "Footnotes"
            safeToHtml $ fromJust fnotes
  where
    metadataElement title friends = H.div ! A.class_ "metab" $ do
            H.span ! A.class_ "metah" $ string title
            friends


entriesTpl context item = do
    entries <- context "entries"
    return $ H.ul ! A.class_ "blogentries" $ safeToHtml entries


-- Splits footnotes from Pandoc generated content and modifies it a bit
-- Modifications include removing parent with .footnotes as well as <hr/>
splitFootnotes :: Item String -> (String, Maybe String)
splitFootnotes = unparse . partition isFns . parse
  where
    classAttr = blank_name { qName = "class" }
    isFns = (==) "footnotes" . fromMaybe "" . findAttr classAttr
    parse = onlyElems . parseXML . itemBody
    unparse ([]  , cEls) = (unparseC cEls, Nothing)
    unparse (fEls, cEls) = (unparseC cEls, Just $ unparseF fEls)
    unparseC = concatMap showElement
    unparseF = concatMap (unparseC . filterChildrenName ((/=) "hr" . qName))
