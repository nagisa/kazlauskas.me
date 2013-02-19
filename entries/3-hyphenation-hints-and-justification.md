---
title: Hyphenation hints and justification
published: 2013-02-18
updated: 2013-02-19
tags: UX
---

# Hyphen and hyphenation

The hyphen is a punctuation mark used to join words and to separate syllables
of a single word. The use of hyphens is called hyphenation[^wiki-thanks]. This
post analyses why one common use case of hyphenation – to break a word so it
continues on another line rather than move the whole word to the next line –
might be preferable in software and web development.

## What is being done today

Nowadays designers and people who produce HTML rarely account[^germans]
for long words and design breakage those words might cause, thus once in a
while you can spot a web page with design broken in unimaginable ways. The
most common solutions to dealing with such breakage include:

1. Trim the text to constant amount of characters and append horizontal
   ellipsis;
2. Insert line breaks every constant amount of characters.

The first solution might be very well acceptable where you have well defined
space restrictions. For example you need to fit a title or heading into
exactly one line which might be or might not be too short to accomodate the
whole text. While it is acceptable for mentioned case it
certainly is not the case for dealing with long words in a block of text as it
makes reading and comprehending text more difficult.

In case of second solution words get split into senseless strings of letters
and depending on environment split word still might be too long to fit into
given space rendering this method useless.

## The solution – hyphenation

Hyphenation will not loose any information like trimming does and hyphenation
follows well understood and defined rules of splitting word into parts. Your
text will be more balanced regardless of text alignment used too, which
contributes to ease of reading.

### Hyphenation in HTML

Before [`hyphens`][hyphens-css] CSS property the only way to make browser
hyphenate words for you was to insert soft hyphens[^soft-hyphen] into the
word at places where the word can be split. At the time of writing CSS
`hyphens` property is still a part of working draft and
[browser support is spotty][hyphens-support]. Therefore using this property is
not feasible yet and soft hyphens are the only reasonable solution. Luckily
there are [algorithms][hyphen-algorithms] as well as convenient libraries for
probably every programming language including [Python][python-hyphen],
[Haskell][haskell-hyphen] and even a client side [JavaScript][js-hyphen]
library to either hyphenate the words or insert soft hyphens into the words
for you!

# Justify all the things

`text-align: justify` does an awesome job at enhancing reading experience
by forcing all lines to be equal in length. Sadly it does so by increasing
the spacing between words therefore failing to enhance the experience.
This way of aligning text may be desired though. Especially in newspaper's
websites and blogs as text in nearly every paper book, newspaper or
other printed medium is justified too.

Web typography blogs warns to be careful with `justify` and recommends
keep `text-align` set to the default alignment – `left`.

Instead of avoiding `justify` which might look better than default, hyphenate
your text and `justify` to your heart's content!

[^wiki-thanks]: [Wikipedia page about hyphen][wiki-hyphen]
[^soft-hyphen]: Soft hyphen codepoint is `U+00AD`. In HTML it also can be used
                with `&shy;` escape.
[^germans]: Big proportion of people who account being German… because of
            average word length in German.

[hyphens-css]: http://www.w3.org/TR/css3-text/#hyphens0
[hyphen-algorithms]: https://en.wikipedia.org/wiki/Hyphenation_algorithm
[wiki-hyphen]: https://en.wikipedia.org/wiki/Hyphen
[hyphens-support]: https://developer.mozilla.org/en-US/docs/CSS/hyphens#Browser_compatibility
[python-hyphen]: http://pypi.python.org/pypi?%3Aaction=search&term=hyphen
[haskell-hyphen]: http://www.haskell.org/hoogle/?hoogle=hyphen
[js-hyphen]: https://code.google.com/p/hyphenator/

[left-hyph-image]: /images/hyphenation-hints-and-justification/left-hyph.png
[left-nohyph-image]: /images/hyphenation-hints-and-justification/left.png
