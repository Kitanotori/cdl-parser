name := "CDL Parser"

version := "1.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "–encoding UTF8", "-target:jvm-1.7")

libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-core" % "1.0.10",
	"ch.qos.logback" % "logback-classic" % "1.0.10",
	"junit" % "junit" % "4.11",
	"org.scalatest" %% "scalatest" % "2.0.M6-SNAP9" % "test"
)
