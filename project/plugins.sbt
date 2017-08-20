name := "auto-diff-build"

resolvers += Classpaths.sbtPluginReleases

resolvers += Classpaths.sbtPluginSnapshots

resolvers += Resolver.sonatypeRepo( "releases" )

resolvers += Resolver.sonatypeRepo( "snapshots" )

addSbtPlugin( "org.scoverage" %% "sbt-scoverage" % "1.5.0" )

addSbtPlugin( "com.eed3si9n" % "sbt-buildinfo" % "0.7.0" )

addSbtPlugin( "com.typesafe.sbt" % "sbt-native-packager" % "1.2.0" )

addSbtPlugin( "com.github.gseitz" % "sbt-release" % "1.0.5" )

addSbtPlugin( "com.timushev.sbt" % "sbt-updates" % "0.3.1" )
