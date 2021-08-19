name := "akka-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.5"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.15",
  "com.typesafe.akka" %% "akka-stream" % "2.6.15",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.6.15" % Test,
  "com.typesafe.akka" %% "akka-http" % "10.2.4",
  
  "co.fs2" %% "fs2-core" % "3.0.3",

  "io.estatico" %% "newtype" % "0.4.4",
  "eu.timepit" %% "refined" % "0.9.25",
  "eu.timepit" %% "refined-cats" % "0.9.25",

  "dev.optics" %% "monocle-core" % "3.0.0",
  "dev.optics" %% "monocle-macro" % "3.0.0",

  "io.circe" %% "circe-core" % "0.14.1",
  "io.circe" %% "circe-generic" % "0.14.1",
  "io.circe" %% "circe-parser" % "0.14.1",
  
  //"org.typelevel" %% "cats-core" % "2.3.0",
  //"org.typelevel" %% "cats-effect" % "2.3.0",
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-effect" % "3.1.1",
  "org.typelevel" %% "cats-mtl" % "1.2.1",

  "tf.tofu" %% "derevo-cats" % "0.12.5",
  "tf.tofu" %% "derevo-cats-tagless" % "0.12.5",
  "tf.tofu" %% "derevo-circe-magnolia" % "0.12.5",
  "tf.tofu" %% "tofu-core-higher-kind" % "0.10.2",

  "dev.optics" %% "monocle-core"  % "3.0.0",
  "dev.optics" %% "monocle-macro" % "3.0.0", // only for Scala 2.13

  "org.http4s" %% "http4s-dsl" % "1.0.0-M23",
  "org.http4s" %% "http4s-blaze-server" % "1.0.0-M23",
  "org.http4s" %% "http4s-blaze-client" % "1.0.0-M23",
  
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.12.0" cross CrossVersion.full)
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-Ymacro-annotations",
  "-Wconf:cat=unused:info"
)
