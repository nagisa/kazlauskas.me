---
title: Aranging ducks
published: 2013-01-19
tags: python
---
Python is as indicated by [official python tutorial][python_tutorial],
the first document most Python newbies encounter, is a dynamically typed
language. While developing I learned to not worry about small details and let
so called duck typing to do it's thing.

[python_tutorial]: http://docs.python.org/3/tutorial/index.html

# The problem

With recent releases, most notably Python 3, dynamism was broken in several
places – most noticeably `str` and `bytes` does not follow this convention
any more. It is, however, understandable as this was done to fix a severe
shortcomings of second version of Python's way of string handling. It is not
just strings though. For this post lets talk about another standard library
function – `range`.

Understanding the purpose of `range` is a simple matter. You give it point at
which you want your arithmetic progression terminate and
optionally what number your sequence should begin with and amount by which
every consecutive number in the sequence should increase.

```python
>>> list(range(0, 10, 3))
    [0, 3, 6, 9]
```

All is fine and good until you decide you want your sequence to increase in
steps of ⅒. I shall try it out by simply passing a `0.1` as last argument.

```python
>>> range(0, 1, 0.1)
    Traceback (most recent call last):
    File "<input>", line 1, in <module>
    TypeError: 'float' object cannot be interpreted as an integer
```

Is it an expected behaviour? Given dynamic behaviour of python it certainly was
not to me. This behaviour drove me crazy for a while, but there is no way it
was done like this without a good reason. A bug? It was the time to dive into
Python sources and look it up.

Strangely enough it was intentional and there was no traces of code to support
any kind of dynamism. The reason of course is performance. Otherwise nobody
would use them in their highly optimised, tight loops, right? On the other hand
inaccuracy of floats as [pointed out][frange] by StackOverflow community might
be yet another reason.

[frange]: http://stackoverflow.com/a/4189798

# Solutions

Now, in case you do not have any problems with pulling in whole `numpy`
library as a dependency you can use [`numpy.arange`][npy_arange]. Otherwise you
may simulate it with list comprehensions or `map`:

```python
>>> [x / 10 for x in range(0, 10)]
    [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
>>> list(map(lambda x: fractions.Fraction(x, 10), range(0, 10)))
    [Fraction(0, 1), Fraction(1, 10), Fraction(1, 5), Fraction(3, 10),
     Fraction(2, 5), Fraction(1, 2), Fraction(3, 5), Fraction(7, 10),
     Fraction(4, 5), Fraction(9, 10)]
```

This solution is functional and terse, however, it is neither convenient, nor
beautiful. Python could do better job here! A wrapper function using regular
`range` given integers and falling back to slower code for everything else
sounds reasonable. At least people would not need to map over values to get
them in form they need.

[npy_arange]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.arange.html#numpy.arange
