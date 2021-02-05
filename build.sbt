import sbtcrossproject.Platform

lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "4.7.0"
lazy val mimaVersion    = "4.7.0" // used for migration-manager

lazy val commonJvmSettings = Seq(
  // dotty started crashing with CyclicReference :-(
  crossScalaVersions := Seq("3.0.0-M3", "2.13.4", "2.12.13"),
)

// sonatype plugin requires that these are in global
ThisBuild / version      := projectVersion
ThisBuild / organization := "de.sciss"

lazy val commonSettings = Seq(
//  version            := projectVersion,
//  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.13.4",
  scalacOptions ++= {
    // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
    // -stars-align produces wrong warnings with decomposing OSC messages
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8")
    val ys = if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
    val sv = scalaVersion.value
    if (sv.startsWith("2.13.")) ys :+ "-Wvalue-discard" else ys
  },
  scalacOptions ++= {
    if (isDotty.value) Nil else Seq("-Xlint:-stars-align,_", "-Xsource:2.13")
  },
  scalacOptions in (Compile, compile) ++= {
    val jvmGt8 = scala.util.Properties.isJavaAtLeast("9")
    val dot    = isDotty.value
    if (!dot && jvmGt8) Seq("-release", "8") else Nil  // JDK >8 breaks API; skip scala-doc
  },
  sources in (Compile, doc) := {
    if (isDotty.value) Nil else (sources in (Compile, doc)).value // dottydoc is complaining about something
  },
  parallelExecution in Test := false,
  concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
  updateOptions      := updateOptions.value.withLatestSnapshots(false),
  testOptions in Test += Tests.Argument("-oF"), // "show full stack traces" (?)
  fork in run := true,  // required for shutdown hook, and also the scheduled thread pool, it seems
  publishConfiguration := publishConfiguration.value.withOverwrite(true), // yeah whatever crossproject plugin bugs
) ++ publishSettings

lazy val deps = new {
  val main = new {
    val asyncFile           = "0.1.2"
    val audioFile           = "2.3.3"
    val equal               = "0.1.6"
    val lucre               = "4.4.1"
    val numbers             = "0.2.1"
    val processor           = "0.5.0"
    val scalaCollider       = "2.6.1"
    val scalaColliderIf     = "1.7.3"
    val scalaColliderUGens  = "1.21.1"
    val scalaJavaTime       = "2.1.0"
    val serial              = "2.0.1"
    val span                = "2.0.2"
    val topology            = "1.1.4"
  }

  val views = new {
    val audioWidgets        = "2.3.2"
    val lucreSwing          = "2.6.1"
    val scalaColliderSwing  = "2.6.1"
    val swingPlus           = "0.5.0"
  }
  
  val test = new {
    val bdb                = "bdb"  // "bdb" or "bdb6" or "bdb7"
    def scalaColliderSwing: String = views.scalaColliderSwing
    val scalaTest          = "3.2.3"
//    val scallop            = "3.5.1"
    val submin             = "0.3.4"
  }
}

lazy val loggingEnabled = true

// ---- modules ----

lazy val platforms = Seq[Platform](JVMPlatform, JSPlatform)

lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(
    synth.jvm, // synth.js, 
    core .jvm, // core .js, 
    views.jvm, // views.js,
    compiler,
  )
//  .dependsOn(synth, core, views, compiler)
  .settings(commonSettings)
  .settings(commonJvmSettings)
  .settings(
    name                  := baseName,
    publish               := {},
    publishArtifact       := false,
    autoScalaLibrary      := false,
    mimaFailOnNoPrevious  := false
  )

lazy val synth = crossProject(platforms: _*).in(file("synth"))
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    name := "Lucre-synth",
    description := "Transactional extension for ScalaCollider",
    libraryDependencies ++= Seq(
      "de.sciss" %%% "topology"                % deps.main.topology,
      "de.sciss" %%% "lucre-core"              % deps.main.lucre,
      "de.sciss" %%% "numbers"                 % deps.main.numbers, // sbt bug
      "de.sciss" %%% "asyncfile"               % deps.main.asyncFile,
      "de.sciss" %%% "audiofile"               % deps.main.audioFile,
      "de.sciss" %%% "scalacollider"           % deps.main.scalaCollider,
      "de.sciss" %%% "scalacolliderugens-core" % deps.main.scalaColliderUGens,
    ),
    Compile / unmanagedSourceDirectories ++= {
      val sourceDirPl = (sourceDirectory in Compile).value
      val sourceDirSh = file(
        sourceDirPl.getPath.replace("/jvm/" , "/shared/").replace("/js/", "/shared/")
      )
      val sv = CrossVersion.partialVersion(scalaVersion.value)
      val (sub1, sub2) = sv match {
        case Some((2, n)) if n >= 13  => ("scala-2.13+", "scala-2.14-")
        case Some((3, _))             => ("scala-2.13+", "scala-2.14+")
        case _                        => ("scala-2.13-", "scala-2.14-")
      }
      Seq(sourceDirPl / sub1, sourceDirPl / sub2, sourceDirSh / sub1, sourceDirSh / sub2)
    },
    mimaPreviousArtifacts := Set("de.sciss" %% "lucre-synth" % mimaVersion)
  )

lazy val testSettings = Seq(
  libraryDependencies += {
    "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test
  }
)

lazy val core = crossProject(platforms: _*).in(file("core"))
  .dependsOn(synth)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-core",
    description := "A framework for creating and managing ScalaCollider based sound processes",
    scalacOptions in Test += "-Yrangepos",  // this is needed to extract source code
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.proc",
    libraryDependencies ++= Seq(
      "de.sciss"          %%% "serial"            % deps.main.serial,
      "de.sciss"          %%% "span"              % deps.main.span,
      "de.sciss"          %%% "lucre-confluent"   % deps.main.lucre,
      "de.sciss"          %%% "lucre-expr"        % deps.main.lucre,
      "de.sciss"          %%% "processor"         % deps.main.processor,
      "de.sciss"          %%% "scalacollider-if"  % deps.main.scalaColliderIf,
//      "de.sciss"          %%  "fileutil"          % deps.main.fileUtil,
      "de.sciss"          %%% "equal"             % deps.main.equal,
//      "org.rogach"        %%% "scallop"           % deps.test.scallop             % Test
    ),
    libraryDependencies += {  // XXX TODO JVM only
      if (isDotty.value) 
        "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
      else 
        "org.scala-lang" %  "scala-compiler"  % scalaVersion.value
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "de.sciss"          %% s"lucre-${deps.test.bdb}"      % deps.main.lucre               % Test,
      "de.sciss"          %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
    ),
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % deps.main.scalaJavaTime,
    )
  )

lazy val views = crossProject(platforms: _*).in(file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-views",
    description := "Views for Sound Processes",
    libraryDependencies ++= Seq(
//      "de.sciss"        %%% "span"                    % deps.main.span,              // sbt bug
      "de.sciss"        %%% "lucre-swing"             % deps.views.lucreSwing,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-views" % mimaVersion)
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "de.sciss"        %% "swingplus"                % deps.views.swingPlus,
      "de.sciss"        %% "audiowidgets-app"         % deps.views.audioWidgets,
      "de.sciss"        %% "scalacolliderswing-core"  % deps.views.scalaColliderSwing,
      "de.sciss"        %  "submin"                   % deps.test.submin    % Test,
      "de.sciss"        %% s"lucre-${deps.test.bdb}"  % deps.main.lucre     % Test,
    ),
    libraryDependencies ++= {
      if (isDotty.value) Nil else Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      )
    },
  )

lazy val compiler = project.withId(s"$baseNameL-compiler").in(file("compiler"))
  .dependsOn(core.jvm, views.jvm)
  .settings(commonSettings)
  .settings(commonJvmSettings)
  .settings(testSettings)
  .settings(
    description := "Compiler-support for Sound Processes",
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in Test := true, // required for compiler
    libraryDependencies ++= Seq(
      "de.sciss"       %% s"lucre-${deps.test.bdb}" % deps.main.lucre               % Test,
      "de.sciss"       %% "lucre-swing"             % deps.views.lucreSwing         % Test,
      "de.sciss"       %% "scalacolliderswing-core" % deps.test.scalaColliderSwing  % Test
    ),
    libraryDependencies += {
      if (isDotty.value)
        "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
      else
        "org.scala-lang" %  "scala-compiler"  % scalaVersion.value
    },
    Compile / unmanagedSourceDirectories ++= {
      val sourceDirPl = (sourceDirectory in Compile).value
      val sv = CrossVersion.partialVersion(scalaVersion.value)
      val (sub1, sub2) = sv match {
        case Some((2, n)) if n >= 13  => ("scala-2.13+", "scala-2.14-")
        case Some((3, _))             => ("scala-2.13+", "scala-2.14+")
        case _                        => ("scala-2.13-", "scala-2.14-")
      }
      Seq(sourceDirPl / sub1, sourceDirPl / sub2)
    },
    Test / unmanagedSourceDirectories ++= {
      val sourceDirPl = (sourceDirectory in Test).value
      val sv = CrossVersion.partialVersion(scalaVersion.value)
      val (sub1, sub2) = sv match {
        case Some((2, n)) if n >= 13  => ("scala-2.13+", "scala-2.14-")
        case Some((3, _))             => ("scala-2.13+", "scala-2.14+")
        case _                        => ("scala-2.13-", "scala-2.14-")
      }
      Seq(sourceDirPl / sub1, sourceDirPl / sub2)
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-compiler" % mimaVersion)
  )

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer(
      id    = "sciss",
      name  = "Hanns Holger Rutz",
      email = "contact@sciss.de",
      url   = url("https://www.sciss.de")
    )
  ),
  scmInfo := {
    val h = "git.iem.at"
    val a = s"sciss/$baseName"
    Some(ScmInfo(url(s"https://$h/$a"), s"scm:git@$h:$a.git"))
  },
)

