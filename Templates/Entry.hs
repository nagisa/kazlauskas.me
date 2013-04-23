{-# LANGUAGE OverloadedStrings #-}
module Templates.Entry
    ( entryTpl
    , entryItemTpl
    , entriesTpl
    ) where

import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid (mempty)
import           Hakyll (MonadMetadata, itemBody, Item(..))
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String
import           Text.XML.Light

import           Templates.Internal

entryItemTpl, entryTpl, entriesTpl  :: MonadMetadata m => Template m String

entryItemTpl context item = do
    date <- context "date" item
    title <- context "title" item
    url <- context "url" item
    return $ renderHtml $
        H.li $ do
            H.time ! A.class_ "blogentry-date" $ toHtml date
            H.a ! A.href (toValue url) $ toHtml title

entryTpl context item = do
    let (body, fnotes) = splitFootnotes item
    date <- context "date" item
    update <- context "updated" item
    tags <- context "tags" item
    toc <- context "toc" item
    return $ renderHtml $ do
        H.aside ! A.id "metadata" $ do
            H.span ! A.class_ "metah" $ toHtml "Post date"
            H.time ! A.pubdate "" $ toHtml date
            H.span ! A.class_ "metah" $ toHtml "Last update"
            H.time $ toHtml update
            H.span ! A.class_ "metah" $ toHtml "Tags"
            H.preEscapedToHtml tags
            if toc == "" then mempty else do
                H.span ! A.class_ "metah" $ toHtml "Table of contents"
                H.preEscapedToHtml toc
        H.article ! A.id "entry" $ H.preEscapedToHtml body
        if fnotes == Nothing then mempty else H.aside ! A.id "footnotes" $ do
            H.h1 $ toHtml "Footnotes"
            H.preEscapedToHtml $ fromJust fnotes

entriesTpl context item = do
    entries <- context "entries" item
    return $ renderHtml $
        H.ul ! A.class_ "blogentries" $ H.preEscapedToHtml entries


-- Splits footnotes from Pandoc generated content and modifies it a bit
-- Modifications include removing parent with .footnotes as well as <hr/>
splitFootnotes :: Item String -> (String, Maybe String)
splitFootnotes item = (content, if fns /= "" then Just fns else Nothing)
  where
    tree = onlyElems . parseXML . itemBody $ item
    isFootnotes = ("footnotes" ==) . fromMaybe "" . findAttr classAttr
    classAttr = blank_name { qName = "class" }
    content = concat . map showElement . filter (not . isFootnotes) $ tree
    fns = concat . map fixFns . filter isFootnotes $ tree
    fixFns = concat . map showElement . filterChildrenName ( ("hr" /=) . qName )
