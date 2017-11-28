name := "auto-diff-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "com.eed3si9n"      % "sbt-buildinfo" % "0.7.0" )
addSbtPlugin( "com.github.gseitz" % "sbt-release"   % "1.0.6" )
addSbtPlugin( "org.scoverage"     % "sbt-scoverage" % "1.5.1" )
addSbtPlugin( "com.lucidchart"    % "sbt-scalafmt"  % "1.14" )
addSbtPlugin( "com.jsuereth"      % "sbt-pgp"       % "1.1.0" )
addSbtPlugin( "org.xerial.sbt"    % "sbt-sonatype"  % "2.0" )
addSbtPlugin( "com.dwijnand"      % "sbt-travisci"  % "1.1.1" )
