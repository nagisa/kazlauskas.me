---
title: Arranging ducks
published: 2013-01-19
updated: 2013-02-18
tags: python
---

Python is, as indicated by [official python tutorial][python_tutorial],
the first document most Python newbies encounter, a dynamically typed
language. While developing I learned to not worry about small details and let
duck typing to do it's thing.

[python_tutorial]: http://docs.python.org/3/tutorial/index.html

# The problem

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

Strangely enough this behaviour is intentional and there are no traces of code
to support any kind of dynamism for this function. The reason of course is
performance. Otherwise nobody would use them in their highly optimised, tight
loops, right? On the other hand inaccuracy of some data types as
[pointed out][frange] by StackOverflow community might be yet another reason.

[frange]: http://stackoverflow.com/a/4189798

# Solutions

Now, in case you do not have any problems with pulling in whole `numpy`
library as a dependency you can use [`numpy.arange`][npy_arange]. Otherwise you
might simulate it with list comprehensions or `map`:

```python
>>> [x / 10 for x in range(0, 10)]
    [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
>>> list(map(lambda x: fractions.Fraction(x, 10), range(0, 10)))
    [Fraction(0, 1), Fraction(1, 10), Fraction(1, 5), Fraction(3, 10),
     Fraction(2, 5), Fraction(1, 2), Fraction(3, 5), Fraction(7, 10),
     Fraction(4, 5), Fraction(9, 10)]
```

This solution is functional and terse, however, it is neither convenient nor
beautiful. Python could do better job here! A wrapper function using regular
`range` given integers and falling back to slower code for everything else
sounds reasonable. At least people would not need to map over values to get
them in form they need.

[npy_arange]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.arange.html#numpy.arange
