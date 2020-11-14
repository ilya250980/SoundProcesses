lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "4.3.0-SNAPSHOT"
lazy val mimaVersion    = "4.3.0" // used for migration-manager

lazy val commonJvmSettings = Seq(
  crossScalaVersions := Seq("2.13.3", "2.12.12"),  // N.B. nsc API has breakage in minor versions (2.13.0 versus 2.13.1)
)

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.13.3",
  scalacOptions ++= {
    // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
    // -stars-align produces wrong warnings with decomposing OSC messages
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint:-stars-align,_", "-Xsource:2.13")
    val ys = if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
    val sv = scalaVersion.value
    if (sv.startsWith("2.13.")) ys :+ "-Wvalue-discard" else ys
  },
  scalacOptions in (Compile, compile) ++= (if (scala.util.Properties.isJavaAtLeast("9")) Seq("-release", "8") else Nil), // JDK >8 breaks API; skip scala-doc
  // resolvers          += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  parallelExecution in Test := false,
  updateOptions      := updateOptions.value.withLatestSnapshots(false),
  testOptions in Test += Tests.Argument("-oF"), // "show full stack traces" (?)
  fork in run := true,  // required for shutdown hook, and also the scheduled thread pool, it seems
  publishConfiguration := publishConfiguration.value.withOverwrite(true), // yeah whatever crossproject plugin bugs
) ++ publishSettings

lazy val deps = new {
  val main = new {
    val asyncFile           = "0.1.1"
    val audioFile           = "2.3.1"
    val equal               = "0.1.6"
//    val fileUtil            = "1.1.5"
    val lucre               = "4.2.0-SNAPSHOT"
    val numbers             = "0.2.1"
    val processor           = "0.5.0"
    val scalaCollider       = "2.4.0"
    val scalaColliderIf     = "1.5.0"
    val span                = "2.0.0"
    val topology            = "1.1.3"
    val ugens               = "1.20.0"
  }

  val views = new {
    val audioWidgets        = "2.2.0"
    val lucreSwing          = "2.3.0-SNAPSHOT"
    val scalaColliderSwing  = "2.3.0"
    val swingPlus           = "0.5.0"
  }
  
  val test = new {
    val bdb                = "bdb"  // "bdb" or "bdb6" or "bdb7"
    def scalaColliderSwing: String = views.scalaColliderSwing
    val scalaTest          = "3.2.3"
    val scallop            = "3.5.1"
    val submin             = "0.3.4"
  }
}

lazy val loggingEnabled = true

// ---- sub-projects ----

lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(
    synth.jvm, synth.js, 
    core .jvm, core .js, 
    views.jvm, views.js,
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

lazy val synth = crossProject(JVMPlatform, JSPlatform).in(file("synth"))
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
      "de.sciss" %%% "scalacolliderugens-core" % deps.main.ugens
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% "lucre-synth" % mimaVersion)
  )

lazy val testSettings = Seq(
  libraryDependencies += {
    "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test
  }
)

lazy val core = crossProject(JVMPlatform, JSPlatform).in(file("core"))
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
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"          %%% "span"              % deps.main.span,              // sbt bug
      "de.sciss"          %%% "lucre-confluent"   % deps.main.lucre,
      "de.sciss"          %%% "lucre-expr"        % deps.main.lucre,
      "de.sciss"          %%% "processor"         % deps.main.processor,
      "de.sciss"          %%% "scalacollider-if"  % deps.main.scalaColliderIf,
//      "de.sciss"          %%  "fileutil"          % deps.main.fileUtil,
      "de.sciss"          %%% "equal"             % deps.main.equal,
      "org.scala-lang"    %   "scala-compiler"    % scalaVersion.value            % Provided,  // XXX TODO JVM only
      "org.rogach"        %%% "scallop"           % deps.test.scallop             % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "de.sciss"          %% s"lucre-${deps.test.bdb}"      % deps.main.lucre               % Test,
      "de.sciss"          %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
    ),
  )

lazy val views = crossProject(JVMPlatform, JSPlatform).in(file("views"))
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
      "org.scala-lang"  %  "scala-reflect"            % scalaVersion.value,
      "de.sciss"        %  "submin"                   % deps.test.submin    % Test,
      "de.sciss"        %% s"lucre-${deps.test.bdb}"  % deps.main.lucre     % Test,
    ),
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
      "org.scala-lang" %  "scala-compiler"          % scalaVersion.value,
      "de.sciss"       %% s"lucre-${deps.test.bdb}" % deps.main.lucre               % Test,
      "de.sciss"       %% "lucre-swing"             % deps.views.lucreSwing         % Test,
      "de.sciss"       %% "scalacolliderswing-core" % deps.test.scalaColliderSwing  % Test
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
  <url>git@git.iem.at:sciss/{baseName}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{baseName}.git</connection>
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
