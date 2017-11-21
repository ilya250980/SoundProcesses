lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "3.16.0"
lazy val mimaVersion    = "3.16.0" // used for migration-manager

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalaVersion       := "2.12.4",
  crossScalaVersions := Seq("2.12.4", "2.11.12"),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  parallelExecution in Test := false
) ++ publishSettings

lazy val lucreVersion               = "3.5.0"
lazy val scalaColliderVersion       = "1.23.0"
lazy val ugensVersion               = "1.17.1"
lazy val scalaColliderIfVersion     = "0.4.0"
lazy val spanVersion                = "1.3.1"
lazy val lucreSwingVersion          = "1.7.0"
lazy val swingPlusVersion           = "0.2.4"
lazy val audioWidgetsVersion        = "1.11.1"
lazy val fileUtilVersion            = "1.1.3"
lazy val topologyVersion            = "1.0.1"
lazy val equalVersion               = "0.1.2"

// ---- test-only ----

lazy val scalaColliderSwingVersion = "1.35.0"
lazy val scalaTestVersion          = "3.0.4"
lazy val loggingEnabled            = true
lazy val bdb                       = "bdb"  // either "bdb" or "bdb6"
lazy val subminVersion             = "0.2.2"
lazy val scoptVersion              = "3.7.0"

scalacOptions in ThisBuild ++= {
  // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
  // -stars-align produces wrong warnings with decomposing OSC messages
  val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")
  val ys = if (scalaVersion.value.startsWith("2.10")) xs else xs :+ "-Xlint:-stars-align,_"  // syntax not supported in Scala 2.10
  if (loggingEnabled || isSnapshot.value) ys else ys ++ Seq("-Xelide-below", "INFO")
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
      "de.sciss" %% "topology"                % topologyVersion,
      "de.sciss" %% "lucre-core"              % lucreVersion,
      "de.sciss" %% "scalacollider"           % scalaColliderVersion,
      "de.sciss" %% "scalacolliderugens-core" % ugensVersion
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% "lucresynth" % mimaVersion)
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core"))
  .dependsOn(synth)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    description := "A framework for creating and managing ScalaCollider based sound processes",
    scalacOptions in Test += "-Yrangepos",  // this is needed to extract source code
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "lucre-confluent"              % lucreVersion,
      "de.sciss"          %% "lucre-expr"                   % lucreVersion,
      "at.iem"            %% "scalacollider-if"             % scalaColliderIfVersion,
      "de.sciss"          %% "fileutil"                     % fileUtilVersion,
      "de.sciss"          %% "equal"                        % equalVersion,
      "org.scala-lang"    %  "scala-compiler"               % scalaVersion.value        % "provided",
      "org.scalatest"     %% "scalatest"                    % scalaTestVersion          % "test",
      "de.sciss"          %% s"lucre-$bdb"                  % lucreVersion              % "test",
      "com.github.scopt"  %% "scopt"                        % scoptVersion              % "test",
      "de.sciss"          %% "scalacolliderswing-plotting"  % scalaColliderSwingVersion % "test"
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
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
      "de.sciss" %% "audiowidgets-app"   % audioWidgetsVersion,
      "de.sciss" %  "submin"             % subminVersion % "test"
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-views" % mimaVersion)
  )

lazy val compiler = Project(id = s"$baseNameL-compiler", base = file("compiler"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    description := "Compiler-support for Sound Processes",
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    libraryDependencies ++= Seq(
      "org.scala-lang" %  "scala-compiler"          % scalaVersion.value,
      "de.sciss"       %% s"lucre-$bdb"             % lucreVersion              % "test",
      "de.sciss"       %% "fileutil"                % fileUtilVersion           % "test",
      "de.sciss"       %% "lucreswing"              % lucreSwingVersion         % "test",
      "de.sciss"       %% "scalacolliderswing-core" % scalaColliderSwingVersion % "test"
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-compiler" % mimaVersion)
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
