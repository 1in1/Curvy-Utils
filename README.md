## Curvy Utils

Some utility scripts I've knocked together while working through a course on elliptic curves. Mostly a bunch of things that are useful for investigation (finding things like rational points, identifying torsion, etc.). Includes a small implementation of finite fields to aid studying reductions.

I've tried to mention references where I've remembered. Might be mistakes, and I won't comment on efficiency. This was much more about helping me learn the content and get through long computations!

Must be built with `-XScopedTypeVariables -XKindSignatures -XDataKinds`. E.g., if you want to play with it in a shell, try

```
ghci Algorthims.hs -XScopedTypeVariables -XKindSignatures -XDataKinds
```
