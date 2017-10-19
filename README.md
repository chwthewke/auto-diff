# Structural diff for scala types

This project provides the `Difference` ADT, which models structural diff of
scala values. It is built (and depends) on
[https://github.com/typelevel/cats](cats) and
[https://github.com/milessabin/shapeless](shapeless).

`Difference`s are computed via the `Diff[A]` type class that wraps a function
of type `(A, A) => Option[Difference]`.
 
The goal of this library is to provide convenient and extensible generic
derivation of `Diff` instances, as well as a decent textual representation
of `Difference` values.

**TODO:** Add examples  

# Current status

- `Diff` instances can be derived for:
  - primitive types as well as `String`,
  - a selection of usual collection types,
  - case classes and sealed hierarchies (via shapeless)
  - enumeratum `Enum`s

- Individual fields of case classes can have their diffing method customized.

- the `Difference` type is still work-in-progress, with a simple colourized
  textual representation and no convenience methods to speak of.

# Future work

- Deriving `Diff` for `Map`s.
- `Option`, `Either` and `Validated` are diffed like any other ADTs, where a
  special purpose instance might serve better.
- Further API exploration for the "front-end", including test framework
  integration.
- Exploring the use of macros to improve `custom`, as well as possibly
  compilation length.
- Extracting the parts that depend on third-party libraries to other modules,
  including the `java.time` part if we cross-build for scala 2.11.

# Credits

[https://github.com/xdotai/diff](xdotai/diff) for inspiration. This code ended
up resembling that more than anticipated.