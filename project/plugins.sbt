name := "auto-diff-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "com.eed3si9n"   % "sbt-buildinfo" % "0.9.0" )
addSbtPlugin( "com.github.sbt" % "sbt-release"   % "1.1.0" )
addSbtPlugin( "org.scoverage"  % "sbt-scoverage" % "1.6.1" )
addSbtPlugin( "org.scalameta"  % "sbt-scalafmt"  % "2.0.1" )
addSbtPlugin( "com.jsuereth"   % "sbt-pgp"       % "1.1.2" )
addSbtPlugin( "org.xerial.sbt" % "sbt-sonatype"  % "2.0" )
