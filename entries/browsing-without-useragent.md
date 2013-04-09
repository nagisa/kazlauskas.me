---
title: Browsing without user agent
published: 2013-03-29 00:00:00+0200
tags: Web, Privacy
---

Recently increased concern about user's privacy is observed in various parts of
the internet. [EFF states][panoptic] that sites are able to pinpoint an user
with high accuracy just from headers sent with every request without actually
keeping any persistent data, like cookies, in user's computer. Some paranoid
people find it to be a problem.

[panoptic]: https://panopticlick.eff.org/index.php

One quiet evening realisation came that there's no way all those headers are
compulsory to use the HTTP protocol. I was pleasantly surprised to find that
the only header required for `HTTP/1.1` is `Host` while `HTTP/1.0` does not
need any at all!  An idea to try browsing the internet without sending an
`User-Agent` was born that evening.

Given how widely `User-Agent` sniffing surfaces in discussions about browser
feature detection I was rather pessimistic about this experiment. I'm glad to
be proven wrong – most of the websites work without batting an eye. I even have
had started a quest of finding one which would actually fail – somebody has to
use UA detection somewhere, right?

The first one to disappoint, after a whole day of browsing, was *BakaBT* – a
half open torrent tracker. The second finding was *Google Mail* which surprised
me quite a bit. I visit Youtube and Picasa Web considerably more often than any
other Google service and they dealt with missing `User-Agent` fine, so I
assumed other Google's services would too. Another interesting finding was that
Google seems to save browser version or some related data in cookies so their
services started failing only after a browser restart[^cookies].

[^cookies]: My browser clears cookies when the browser is closed

After discovering that *Mail* fails, I started checking all other
Google services to see if they failed too – mostly with saddening results.

*Admin Control Panel* displays `Loading…` forever because it is unable
to fetch a single JavaScript file named `undefined.cache.js`.

Opening *Drive* front page simply complains about file not existing, which is a
bit strange as I wasn't opening any file. Opening links to shared files does
work though. From the preview screen one can then create a simple document.

*Maps* application loads fine, but map tiles fail to download with `HTTP 403`
(?vector=1, which supposedly enables WebGL maps didn't work as well). Treasure
mode works though.

Such minor mistakes render these services completely unusable.
There's some that fail more gracefully:

* *Calendar* which redirects to a mobile version;
* *Groups* serves the old version and lets user to opt into a new one, which
  works fine if you ignore a scary alert about problem with *.gwt.xml file;
* *Picasa Web* just shows a warning that the browser is not supported, but
  otherwise works completely fine;
* *Contacts* and *Youtube* work without any noticeable differences.

All in all, if you care about privacy and use something else than Google,
browsing without sending `User-Agent` is feasible. In case you prefer
Google, then it's not like you care about your privacy after all, do you?

A continiously updated list of websites failing this experiment:

* [tools.ietf.org](https://tools.ietf.org/)
* [git.gnome.org](https://git.gnome.org/)
* facebook.com (Don't feel generous enough to link this one for them)
* [arxiv.org](http://arxiv.org) (The only one to give a proper explanation on
  error page so far)
* [code.google.com/p/chromium/](https://code.google.com/p/chromium/issues)
  (Issue reporting button does not work. Specific to chromium project)
