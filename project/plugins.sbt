name := "auto-diff-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "com.eed3si9n"      % "sbt-buildinfo"       % "0.7.0" )
addSbtPlugin( "com.github.gseitz" % "sbt-release"         % "1.0.6" )
addSbtPlugin( "com.typesafe.sbt"  % "sbt-native-packager" % "1.3.2" )
addSbtPlugin( "com.timushev.sbt"  % "sbt-updates"         % "0.3.3" )
addSbtPlugin( "org.scoverage"     %% "sbt-scoverage"      % "1.5.1" )
addSbtPlugin( "com.lucidchart"    % "sbt-scalafmt"        % "1.14" )
