# silica

Haskell optics with type errors useful enough to teach people.

```
$ nix-shell
$ cabal repl

*Silica> [(x, 3 * x) | x <- [1..10]]
[(1,3),(2,6),(3,9),(4,12),(5,15),(6,18),(7,21),(8,24),(9,27),(10,30)]

*Silica> [(x, 3 * x) | x <- [1..10]] & productOf _2

<interactive>:5:41: error:
    • You tried to access the second field of a list.
      However, a list does not have any "fields". Tuples and similar types can.

      You have a list of tuples of type (Integer, Integer).
      Try applying `folded` or a similar combinator to first traverse "into" the list.
      Then you can use field selector lenses like _1 to access the fields of the tuples inside.

      For example,
      >>> [(1,1),(2,4),(3,7)] & sumOf (folded % _2)
      12

      >>> [(1,1),(2,4),(3,7)] & sumOf _2
      <this error>

      Use `folded` as many times as you need to to drill down into nested structures.
      For example, here's a nested list:
      >>> [[(1,1),(2,4),(3,7)],[(5,6)],[(2,1),(4,3)]] & sumOf (folded % folded % _2)
      22

    • In the first argument of ‘productOf’, namely ‘_2’
      In the second argument of ‘(&)’, namely ‘productOf _2’
      In the expression: [(x, 3 * x) | x <- [1 .. 10]] & productOf _2

*Silica> [(x, 3 * x) | x <- [1..10]] & productOf (folded % _2)
214277011200
```

# Credits

Most of the code is from Kmett's original [lens](https://github.com/ekmett/lens/) library and [well-typed/optics](https://github.com/well-typed/optics/), with the latter providing the ideas for the API and the former the internals of the implementation.
