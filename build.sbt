libraryDependencies ++= Seq(
  "co.fs2"               %% "fs2-core"      % "1.0.0-M5",
  "org.typelevel"        %% "cats-core"     % "1.4.0",
  "org.typelevel"        %% "cats-mtl-core" % "0.3.0",
  "com.github.mpilquist" %% "simulacrum"    % "0.13.0",
  "com.olegpy"           %% "meow-mtl"      % "0.1.2",
  "org.scalatest"        %% "scalatest"     % "3.0.0" % Test,
  compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)),
  compilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.7"),
  compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.0-M2")
)

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-feature"
)

name := "classy-playground"
