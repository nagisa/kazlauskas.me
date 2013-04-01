---
title: Hyphenation hints and justification
published: 2013-02-18
updated: 2013-02-19
tags: UX, Web
---

# Hyphen and hyphenation

<q cite="https://en.wikipedia.org/wiki/Hyphen">The hyphen is a punctuation mark
used to join words and to separate syllables of a single word. The use of
hyphens is called hyphenation.</q> A common use case of hyphenation – to break
a word so it continues on another line rather than move the whole word to the
next line – might be preferable in web pages for the same reasons it is used
in the print media.

## What is being done today

Nowadays designers and people who produce HTML rarely account[^germans]
for long words and design breakage those words might cause, thus once in a
while people spot a web page with design broken in unimaginable ways. The
most common ~~solutions~~ dirty hacks to dealing with such breakage include

* Trimming the text to constant amount of characters and appending a
  horizontal ellipsis;
* Inserting line breaks every constant amount of characters.

The first solution might be very well acceptable where you have well defined
space restrictions. For example you need to fit a title or heading into
exactly one line, which might be or might not be too short to accommodate the
whole text. While the hack is acceptable for mentioned case it certainly is
not for dealing with the words in a block of text as reading and
comprehending content becomes more difficult.

The second solution is acceptable when a fixed-width font is used and all lines
are equal in length. In all other cases the text gets split into seemingly
unrelated, senseless lines and the reader in order to comprehend the text has
to understand the unusual splitting method first.

[^germans]: Big proportion of people who account being German… because of
            average word length in German

## The solution – hyphenation

Hyphenation, on the other hand, follows well understood and defined rules of
splitting word into parts and does not loose any information like trimming
does. In addition the text becomes more balanced regardless of text
alignment used, which contributes to the ease of reading.

### Hyphenation in HTML

Before [`hyphens`][hyphens-css] CSS property the only way to make browser
hyphenate words was to inject soft hyphens[^soft-hyphen] into the word where
hyphenation is allowed. At the time of writing `hyphens` property is still a
part of working draft and
[browser support is spotty][hyphens-support], therefore using this property is
not feasible yet and insertion of soft hyphens is the only reasonable solution. Luckily
[hyphenation algorithms][hyphen-algorithms] as well as convenient libraries
that implement the algorithms exist. Probably every programming language
including [Python][python-hyphen], [Haskell][haskell-hyphen] and even a client
side [JavaScript][js-hyphen] has a library to either hyphenate the words or
inject soft hyphens into the words for you!

[^soft-hyphen]: Code point of soft hyphen is `U+00AD`. In HTML it also can be
                used with `&shy;` escape

[hyphens-css]: http://www.w3.org/TR/css3-text/#hyphens0
[hyphens-support]: https://developer.mozilla.org/en-US/docs/CSS/hyphens#Browser_compatibility
[hyphen-algorithms]: https://en.wikipedia.org/wiki/Hyphenation_algorithm
[python-hyphen]: http://pypi.python.org/pypi?%3Aaction=search&term=hyphen
[haskell-hyphen]: http://www.haskell.org/hoogle/?hoogle=hyphen
[js-hyphen]: https://code.google.com/p/hyphenator/

# Justify all the things

Justified the text may be desired, especially in newspaper's websites and
blogs as the text in nearly every paper book, newspaper and other print media
is justified too. `text-align: justify` does just that, however method to
justify a line is not flawless and to make justified text look better, words
at the end of line should be broken as well (soft hyphens should be
injected).

Now you can ignore all the silly web typography blogs' recommendations and
instead of avoiding `justify`, hyphenate your text and `justify` to your
heart's content!
