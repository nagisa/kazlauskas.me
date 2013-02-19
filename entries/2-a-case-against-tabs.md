---
title: A case against tabs
published: 2013-01-23
updated: 2013-02-09
tags: UX, GNOME
---

A long time (12 years or so) ago certain two browsers were fighting over
market share and neither of them featured a certain UI element which is
inseparatable part of today's browser designs. Nor did most other applications
had it. Then, while being anihilated, Netscape added tabs as a last struggle.
That is where things started going very wrong.

Opera added them just several months later and interestingly enough IE, the
culprit behind disappearance of Netscape, got them only four years later with
seventh version. Tabs by then already has become all the rage and plague
escaped the little browsers' playground. Even applications that doesn't
get any real benefit from tabs implemented them.

# Windows have graduated

Looking back it becomes clear why tabs were coined at all. Back then your
window manager would just make sure your window is drawn on the screen and add
window border. Anything else was your own responsibility. No window grouping
(or even compositing) at all.

Today, if I recall correctly in Windows® all windows are neatly grouped
together by application in the taskbar and clicking on such a group will reveal
a list of open windows for that specific application. You should already see
how this functionality duplicates the purpose of tabs. In my opinion this is a
great solution to handling tab like interface at OS level.

Likewise you find applications instead of windows as first level citizens in
OS X, GNOME, Unity and I suspect in most other modern desktop environments.
Actually working with GTK+, the toolkit used to make GNOME applications,
the first object you would create is not a window anymore. It's a
`GtkApplication`.

So in case tabs were created to compensate for a missing layer of separation
they did their job well and now is the time for them to retire.

# Implementation details

Now we all know too well that tabs never behave as any other UI element managed
by OS. When you have windows you can be sure you'll close them with `Alt+F4`
unless application is doing something nasty. `Alt+Tab` you will change between
active applications and with `Alt+a button above Tab` between windows of
application. You never get consistent behaviour with tabs though. The shortcut
to open a tab in Application 1 may very well remove an important directory in
another application. We cannot expect same level of shortcut uniformity with
tabs unless they become a integral part of OS, just like windows and
applications now are.

Sure the shortcuts to interact with tabs will very likely be the same
in same application in all operating systems (given the application is portable
at all), but on the other hand that means you have to keep different
sets of shortcuts in your head for every different application you use.

# Implementor's hell

Tabs seem to be a clear violation of DRY principle as every application
has it's own implementation of tab functionality. Either that or they use
toolikt widgets not meant for that. For example `GtkNotebook` was never meant
to be a contraption of today's tabbed interfaces and was designed with
different requirements in mind. Still due to it's similarity it is widely used
as a good enough solution.

# Close button, close button and close button

Tabbed interfaces also introduce more controls. One more close button in
addition to close button for window is one good example of higher complexity.
You have to take care to close only those windows that does not have important
tabs open too and as a consecuence you always get to answer to dialog if you
really want to close a window.

# In Conclusion

Tabs:

* Makes user learning curve steeper;
* Unpredictable – as applications implement tabbing themselves the, supposed
  to be, common knowledge becomes useless between different applications –
  there's no guarantee for common functionality, shortcuts, position or even
  look;
* Not integrated into OS experience;
* Unneccesary – they were coined to alleviate the problem of missing layer of
  separation which is fixed now;
* Introduce additional development overhead.

I hope wouldn't be wrong by stating that ~~most~~ all UX designers agree with
me that confusing UI is a disaster and tabs as they are now are just that.

# Also some ¢s from me

Having went tab-less at least half a year ago I can confidently affirm that
I would not want to go back to using tabs anytime foreseeable.