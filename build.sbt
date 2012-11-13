name := "CDL Parser"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "–encoding UTF8", "-target:jvm-1.5")

libraryDependencies ++= Seq(
	"ch.qos.logback" % "logback-core" % "1.0.7",
	"ch.qos.logback" % "logback-classic" % "1.0.7",
	"junit" % "junit" % "4.10",
	"org.scalatest" % "scalatest_2.9.2" % "2.0.M4" % "test"
)
