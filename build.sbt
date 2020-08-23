name := "PhiloFinder"

version := "0.1"

scalaVersion := "2.12.4"


libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"
resolvers += Resolver.sonatypeRepo("releases")
libraryDependencies += "com.danielasfregola" %% "twitter4s" % "6.2"
libraryDependencies += "ch.qos.logback"      % "logback-classic" % "1.1.9"


mainClass in assembly := Some("net.zyuiop.philofinder.Twitter")