name := "first"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings", // ? fail the compilation if there are any warnings
  "-Ypartial-unification"
  /*
   it's for better type inference when multiple type parameters are involved
   and they need to be inferred in multiple steps,
   usually due to the use of higher-kinded types (which appear in many functional libraries).
   */
)