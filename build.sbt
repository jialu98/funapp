name := "funapp"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    exclude("org.scala-lang.modules", "scala-xml_2.11")
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
    