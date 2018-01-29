lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "3.17.0"
lazy val mimaVersion    = "3.17.0" // used for migration-manager

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

lazy val deps = new {
  val main = new {
    val lucre               = "3.5.0"
    val scalaCollider       = "1.23.0"
    val ugens               = "1.17.1"
    val scalaColliderIf     = "0.4.0"
    val span                = "1.3.1"
    val lucreSwing          = "1.7.0"
    val swingPlus           = "0.2.4"
    val audioWidgets        = "1.11.2"
    val fileUtil            = "1.1.3"
    val topology            = "1.1.0"
    val equal               = "0.1.2"
    val model               = "0.3.4"
  }
  
  val test = new {
    val scalaColliderSwing = "1.35.0"
    val scalaTest          = "3.0.4"
    val bdb                = "bdb"  // either "bdb" or "bdb6"
    val submin             = "0.2.2"
    val scopt              = "3.7.0"
  }
}

lazy val loggingEnabled = true


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
      "de.sciss" %% "topology"                % deps.main.topology,
      "de.sciss" %% "lucre-core"              % deps.main.lucre,
      "de.sciss" %% "scalacollider"           % deps.main.scalaCollider,
      "de.sciss" %% "scalacolliderugens-core" % deps.main.ugens
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
      "de.sciss"          %% "lucre-confluent"              % deps.main.lucre,
      "de.sciss"          %% "lucre-expr"                   % deps.main.lucre,
      "at.iem"            %% "scalacollider-if"             % deps.main.scalaColliderIf,
      "de.sciss"          %% "fileutil"                     % deps.main.fileUtil,
      "de.sciss"          %% "equal"                        % deps.main.equal,
//      "de.sciss"          %% "model"                        % deps.main.model, // sbt bug
      "org.scala-lang"    %  "scala-compiler"               % scalaVersion.value        % "provided",
      "org.scalatest"     %% "scalatest"                    % deps.test.scalaTest          % "test",
      "de.sciss"          %% s"lucre-${deps.test.bdb}"      % deps.main.lucre              % "test",
      "com.github.scopt"  %% "scopt"                        % deps.test.scopt              % "test",
      "de.sciss"          %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing % "test"
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val views = Project(id = s"$baseNameL-views", base = file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    description := "Views for Sound Processes",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing"         % deps.main.lucreSwing,
      "de.sciss" %% "swingplus"          % deps.main.swingPlus,
      "de.sciss" %% "audiowidgets-swing" % deps.main.audioWidgets,
      "de.sciss" %% "audiowidgets-app"   % deps.main.audioWidgets,
      "de.sciss" %  "submin"             % deps.test.submin % "test"
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
      "de.sciss"       %% s"lucre-${deps.test.bdb}" % deps.main.lucre               % "test",
//      "de.sciss"       %% "fileutil"                % deps.main.fileUtil            % "test",
      "de.sciss"       %% "lucreswing"              % deps.main.lucreSwing          % "test",
      "de.sciss"       %% "scalacolliderswing-core" % deps.test.scalaColliderSwing  % "test"
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
