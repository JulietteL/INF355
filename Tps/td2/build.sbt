scalaVersion := "2.11.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6"


scalaVersion := "2.11.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.3.12" % "test",
			    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
                            "com.netflix.rxjava" % "rxjava-scala" % "0.18.3")

resolvers += "releases"  at "http://oss.sonatype.org/content/repositories/releases"

scalacOptions ++= Seq("-deprecation", "-feature")
