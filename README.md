[![Build Status](https://travis-ci.org/chwthewke/auto-diff.svg?branch=master)](https://travis-ci.org/chwthewke/auto-diff)
[![codecov.io](http://codecov.io/github/chwthewke/auto-diff/coverage.svg?branch=devel)](http://codecov.io/github/chwthewke/auto-diff?branch=devel)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/fr.thomasdufour/auto-diff-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/fr.thomasdufour/auto-diff-core_2.12)

# Structural diff for scala types

This project provides the `Difference` ADT, which models structural diff of
scala values. It is built (and depends) on
[cats](https://github.com/typelevel/cats) and
[shapeless](https://github.com/milessabin/shapeless).

`Difference`s are computed via the `Diff[A]` type class that wraps a function
of type `(A, A) => Option[Difference]`.
 
The goal of this library is to provide convenient and extensible generic
derivation of `Diff` instances, as well as a decent textual representation
of `Difference` values.

**TODO:** Add examples, with tut ideally

# Current status

- `Diff` instances can be derived for:
  - primitive types as well as `String`, `UUID`,
  - a selection of usual collection-like types, including `Map`, `Option` and `Either`
  - enumeratum `Enum`s, via `autodiff-enumeratum`.

- Generic derivation (for `LabelledGeneric` types) is provided by `autodiff-generic`, and is opt-in like e.g. in [circe](https://circe.io)
  - either automatic with `import fr.thomasdufour.autodiff.generic.auto._`
  - or semi-automatic with `import fr.thomasdufour.autodiff.generic.semiauto._` and using `deriveDiff`

- The `Difference` type has only a couple textual representation options 
  and no convenience methods to speak of.

# Future work

- Further API exploration for the "front-end", including test framework
  integration.
- Improve test coverage

# Credits

[xdotai/diff](https://github.com/xdotai/diff) for inspiration.
