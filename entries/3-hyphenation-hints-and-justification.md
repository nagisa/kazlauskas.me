---
title: Hyphenation hints and justification
published: 2013-02-18
updated: 2013-02-18
tags: HTML, UX
---

# Hyphenation in HTML

Before [`hyphens`][hyphens-css] CSS property (which at the time of writing
is still a working draft, therefore [not feasible][hyphens-support] yet) the
only way to make browser hyphenate words is to insert soft hyphens (`U+00AD`
or `&shy;`) in the word where it is OK to hyphenate. And luckily enough
there are [algorithms][hyphen-algorithms] as well as convenient libraries for
[Python][python-hyphen], [Haskell][haskell-hyphen] and even a client side
[JavaScript][js-hyphen] library to do that for you!

## Why should you care about hyphenation?

Common solutions to dealing with very long words (in comments) are:

1. Trim the word to constant amount of characters and append horizontal
   ellipsis;
2. [Insert line breaks][php-wordwrap] every constant amount of characters.

The first solution is lossy and should only be used where you have well defined
space restrictions. The second one will not always break the word in the
correct position and will always cause some confusion until the reader will
figure out that the word is split. Hyphenation on the other hand provides a
well understood way of splitting long and short words alike into another line.

![Hyphenated text line length is more equal][left-hyph-image]
![Non-hyphenated text line length varies greatly][left-nohyph-image]

Your text will also be more balanced regardless of text alignment used.
Therefore hyphenated text looks better and at least to me is easier to read.

# Justify all the things

`text-align: justify` does an awesome job at enchancing reading experience
by forcing all lines to be equal in length. Sadly it does so by increasing
the spacing between words – this method might fail in several settings.
This way of aligning text may be desired though. Especially in newspaper's
websites and blogs as text in nearly every paper book, newspaper or
other printed medium is justified too.

Web typography blogs warns to be careful with `justify` and recommends
keep `text-align` set to the default alignment – `left`. Instead of avoiding
`justify` which might look better than default, hyphenate your text and
`justify` to your heart's content!

[hyphens-css]: http://www.w3.org/TR/css3-text/#hyphens0
[hyphen-algorithms]: https://en.wikipedia.org/wiki/Hyphenation_algorithm
[hyphens-support]: https://developer.mozilla.org/en-US/docs/CSS/hyphens#Browser_compatibility
[python-hyphen]: http://pypi.python.org/pypi?%3Aaction=search&term=hyphen
[haskell-hyphen]: http://www.haskell.org/hoogle/?hoogle=hyphen
[js-hyphen]: https://code.google.com/p/hyphenator/
[php-wordwrap]: http://www.php.net/manual/en/function.wordwrap.php

[left-hyph-image]: /images/hyphenation-hints-and-justification/left-hyph.png
[left-nohyph-image]: /images/hyphenation-hints-and-justification/left.png
