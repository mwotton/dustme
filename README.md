Dustme
======

This is a reimplementation of Gary Bernhardt's
[Selecta](https://github.com/garybernhardt/selecta)
in Haskell. It is a bit faster, approximately half the number of lines
of code, and you only need to deploy one binary rather than the script
and a ruby interpreter. (Try

```
cat /usr/share/dict/words | selecta
```

and

```
cat /usr/share/dict/words | dustme
```

to compare - dustme is considerably snappier.)

While it's still not finished (Selecta's colouring method is more
robust under different terminals), it's usable.

PRs & bug reports welcome.
