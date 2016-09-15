lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "3.6.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalaVersion       := "2.11.8",
  crossScalaVersions := Seq("2.11.8", "2.10.6"),
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat
) ++ publishSettings

lazy val lucreVersion               = "3.3.1"
lazy val scalaColliderVersion       = "1.20.1"
lazy val scalaColliderIfVersion     = "0.1.0-SNAPSHOT"
lazy val spanVersion                = "1.3.1"
lazy val lucreSwingVersion          = "1.4.0"
lazy val swingPlusVersion           = "0.2.1"
lazy val audioWidgetsVersion        = "1.10.0"
lazy val fileUtilVersion            = "1.1.2"
lazy val topologyVersion            = "1.0.0"

// ---- test-only ----

lazy val scalaColliderSwingVersion = "1.30.0"
lazy val scalaTestVersion          = "3.0.0"
lazy val loggingEnabled            = true
lazy val bdb                       = "bdb"  // either "bdb" or "bdb6"
lazy val scoptVersion              = "3.5.0"

scalacOptions in ThisBuild ++= {
  // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
  // -stars-align produces wrong warnings with decomposing OSC messages
  val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture",
    "-Xlint:-stars-align,_")
  if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
}

// SI-7481
// scalacOptions += "-no-specialization"

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- sub-projects ----

lazy val root = Project(id = baseNameL, base = file("."))
  .aggregate(synth, core, views, compiler)
  .dependsOn(synth, core, views, compiler)
  .settings(commonSettings)
  .settings(
    name := baseName,
    publishArtifact in(Compile, packageBin) := false, // there are no binaries
    publishArtifact in(Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in(Compile, packageSrc) := false, // there are no sources
    // packagedArtifacts := Map.empty
    autoScalaLibrary := false
  )

lazy val synth = Project(id = "lucresynth", base = file("synth"))
  .settings(commonSettings)
  .settings(
    description := "Transactional extension for ScalaCollider",
    libraryDependencies ++= Seq(
      "de.sciss" %% "topology"        % topologyVersion,
      "de.sciss" %% "lucre-core"      % lucreVersion,
      "de.sciss" %% "scalacollider"   % scalaColliderVersion
    )
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core"))
  .dependsOn(synth)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    description := "A framework for creating and managing ScalaCollider based sound processes",
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "lucre-confluent"  % lucreVersion,
      "de.sciss"          %% "lucre-expr"       % lucreVersion,
      "at.iem.sysson"     %% "scalacollider-if" % scalaColliderIfVersion,
      "de.sciss"          %% "fileutil"         % fileUtilVersion,
      "org.scalatest"     %% "scalatest"        % scalaTestVersion  % "test",
      "de.sciss"          %% s"lucre-$bdb"      % lucreVersion      % "test",
      "com.github.scopt"  %% "scopt"            % scoptVersion      % "test",
      "de.sciss"          %% "scalacolliderswing-plotting" % scalaColliderSwingVersion % "test"
    )
  )

lazy val views = Project(id = s"$baseNameL-views", base = file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    description := "Views for Sound Processes",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing"         % lucreSwingVersion,
      "de.sciss" %% "swingplus"          % swingPlusVersion,
      "de.sciss" %% "audiowidgets-swing" % audioWidgetsVersion,
      "de.sciss" %% "audiowidgets-app"   % audioWidgetsVersion
    )
  )

lazy val compiler = Project(id = s"$baseNameL-compiler", base = file("compiler"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    description := "Compiler-support for Sound Processes",
    libraryDependencies ++= Seq(
      "org.scala-lang" %  "scala-compiler"          % scalaVersion.value,
      "de.sciss"       %% s"lucre-$bdb"             % lucreVersion              % "test",
      "de.sciss"       %% "fileutil"                % fileUtilVersion           % "test",
      "de.sciss"       %% "lucreswing"              % lucreSwingVersion         % "test",
      "de.sciss"       %% "scalacolliderswing-core" % scalaColliderSwingVersion % "test"
    )
  )

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
<scm>
  <url>git@github.com:Sciss/{baseName}.git</url>
  <connection>scm:git:git@github.com:Sciss/{baseName}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)
