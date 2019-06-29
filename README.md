[![Build Status](https://travis-ci.org/chwthewke/auto-diff.svg?branch=master)](https://travis-ci.org/chwthewke/auto-diff)
[![codecov.io](http://codecov.io/github/chwthewke/auto-diff/coverage.svg?branch=master)](http://codecov.io/github/chwthewke/auto-diff?branch=master)
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

```scala
    case class Address( street: String, city: String )
    case class Person( name: String, age: Int, address: Address )

    import fr.thomasdufour.autodiff._

    implicit val personDiff: Diff[Person] = {
      import derived.auto._
      derived.semi.diff
    }

    Pretty.colorized2.showDiff(
      Person( "Jean Martin", 29, Address( "2 rue Pasteur", "Lille" ) ),
      Person( "Jean Martin", 55, Address( "2 rue Pasteur", "Lyon" ) )
    )
```

yields (with some ANSI coloring that I can't figure how to add here):

```
in Person
- age
  - 29 -> 55
- address
  - in Address
    - city
      - Lille -> Lyon
```

# How to use auto-diff

Only Scala 2.12 is supported at the moment.

Add some or all of the following to your `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "fr.thomasdufour" %% "auto-diff-core" % "x.y.z",
  "fr.thomasdufour" %% "auto-diff-generic" % "x.y.z",
  "fr.thomasdufour" %% "auto-diff-enumeratum" % "x.y.z",
  "fr.thomasdufour" %% "auto-diff-scalatest" % "x.y.z" % "test"
)
```

For the current version, check the maven central badge at the top of this readme.
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
