lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "4.0.0-SNAPSHOT"
lazy val mimaVersion    = "4.0.0" // used for migration-manager

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.13.3",
  crossScalaVersions := Seq("2.13.3", "2.12.12"),  // N.B. nsc API has breakage in minor versions (2.13.0 versus 2.13.1)
  scalacOptions ++= {
    // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
    // -stars-align produces wrong warnings with decomposing OSC messages
    val ys = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint:-stars-align,_", "-Xsource:2.13")
    if (loggingEnabled || isSnapshot.value) ys else ys ++ Seq("-Xelide-below", "INFO")
  },
  scalacOptions in (Compile, compile) ++= (if (scala.util.Properties.isJavaAtLeast("9")) Seq("-release", "8") else Nil), // JDK >8 breaks API; skip scala-doc
  // resolvers          += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  parallelExecution in Test := false,
  updateOptions      := updateOptions.value.withLatestSnapshots(false),
  testOptions in Test += Tests.Argument("-oF"), // "show full stack traces" (?)
  fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems
) ++ publishSettings

lazy val deps = new {
  val main = new {
    val audioFile           = "2.0.0"
    val audioWidgets        = "2.0.0"
    val equal               = "0.1.5"
    val fileUtil            = "1.1.5"
    val lucre               = "4.0.0-SNAPSHOT"
    val lucreSwing          = "2.0.0-SNAPSHOT"
    val numbers             = "0.2.1"
    val scalaCollider       = "2.0.0"
    val scalaColliderIf     = "1.0.0"
    val span                = "2.0.0"
    val swingPlus           = "0.4.2"
    val topology            = "1.1.3"
    val ugens               = "1.19.8"
  }

  val views = new {
    val scalaColliderSwing = "2.0.0"
  }
  
  val test = new {
    val bdb                = "bdb"  // "bdb" or "bdb6" or "bdb7"
    def scalaColliderSwing: String = views.scalaColliderSwing
    val scalaTest          = "3.2.2"
    val scallop            = "3.5.1"
    val submin             = "0.3.4"
  }
}

lazy val loggingEnabled = true

// ---- sub-projects ----

lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(synth, core, views, compiler)
  .dependsOn(synth, core, views, compiler)
  .settings(commonSettings)
  .settings(
    name := baseName,
    publishArtifact in(Compile, packageBin) := false, // there are no binaries
    publishArtifact in(Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in(Compile, packageSrc) := false, // there are no sources
    // packagedArtifacts := Map.empty
    autoScalaLibrary := false,
    mimaFailOnNoPrevious := false
  )

lazy val synth = project.withId("lucre-synth").in(file("synth"))
  .settings(commonSettings)
  .settings(
    description := "Transactional extension for ScalaCollider",
    libraryDependencies ++= Seq(
      "de.sciss" %% "topology"                % deps.main.topology,
      "de.sciss" %% "lucre-core"              % deps.main.lucre,
      "de.sciss" %% "numbers"                 % deps.main.numbers, // sbt bug
      "de.sciss" %% "audiofile"               % deps.main.audioFile,
      "de.sciss" %% "scalacollider"           % deps.main.scalaCollider,
      "de.sciss" %% "scalacolliderugens-core" % deps.main.ugens
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% "lucre-synth" % mimaVersion)
  )

lazy val testSettings = Seq(
  libraryDependencies += {
    "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test
  }
)

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .dependsOn(synth)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    description := "A framework for creating and managing ScalaCollider based sound processes",
    scalacOptions in Test += "-Yrangepos",  // this is needed to extract source code
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "span"                         % deps.main.span,              // sbt bug
      "de.sciss"          %% "lucre-confluent"              % deps.main.lucre,
      "de.sciss"          %% "lucre-expr"                   % deps.main.lucre,
      "de.sciss"          %% "scalacollider-if"             % deps.main.scalaColliderIf,
      "de.sciss"          %% "fileutil"                     % deps.main.fileUtil,
      "de.sciss"          %% "equal"                        % deps.main.equal,
      "org.scala-lang"    %  "scala-compiler"               % scalaVersion.value            % Provided,
      "de.sciss"          %% s"lucre-${deps.test.bdb}"      % deps.main.lucre               % Test,
      "de.sciss"          %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
      "org.rogach"        %% "scallop"                      % deps.test.scallop             % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val views = project.withId(s"$baseNameL-views").in(file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    description := "Views for Sound Processes",
    libraryDependencies ++= Seq(
      "de.sciss"        %% "span"                     % deps.main.span,              // sbt bug
      "de.sciss"        %% "lucre-swing"              % deps.main.lucreSwing,
      "de.sciss"        %% "swingplus"                % deps.main.swingPlus,
      "de.sciss"        %% "audiowidgets-app"         % deps.main.audioWidgets,
      "de.sciss"        %% "scalacolliderswing-core"  % deps.views.scalaColliderSwing,
      "org.scala-lang"  %  "scala-reflect"            % scalaVersion.value,
      "de.sciss"        %  "submin"                   % deps.test.submin    % Test,
      "de.sciss"        %% s"lucre-${deps.test.bdb}"  % deps.main.lucre     % Test,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-views" % mimaVersion)
  )

lazy val compiler = project.withId(s"$baseNameL-compiler").in(file("compiler"))
  .dependsOn(core, views)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    description := "Compiler-support for Sound Processes",
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in Test := true, // required for compiler
    libraryDependencies ++= Seq(
      "org.scala-lang" %  "scala-compiler"          % scalaVersion.value,
      "de.sciss"       %% s"lucre-${deps.test.bdb}" % deps.main.lucre               % Test,
      "de.sciss"       %% "lucre-swing"             % deps.main.lucreSwing          % Test,
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
