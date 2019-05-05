lazy val baseName           = "LucreMatrix"
lazy val baseNameL          = baseName.toLowerCase

lazy val projectVersion     = "1.7.0-SNAPSHOT"
lazy val mimaVersion        = "1.7.0"

lazy val scalaMainVersion   = "2.12.8"

lazy val deps = new {
  val core = new {
    val audioFile   = "1.5.3"
    val fileCache   = "0.5.1"
    val fscape      = "2.25.0"
    val lucre       = "3.12.0"
    val netCDF      = "4.6.13"
  }
  val views = new {
    val lucreSwing  = "1.16.0"
  }
  val test = new {
    val scalaTest   = "3.0.7"
    val submin      = "0.2.5"
  }
}

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := scalaMainVersion,
  crossScalaVersions := Seq(scalaMainVersion, "2.11.12"),
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  licenses           := Seq("LGPL v2.1+" -> url("https://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13"),
  resolvers         ++= Seq(
    "Oracle Repository" at "http://download.oracle.com/maven",                                          // required for sleepycat
     "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"   // required for NetCDF
  ),
  // ---- publishing ----
  publishMavenStyle := true,
  publishTo       := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = baseName
    <scm>
      <url>git@git.iem.at:sciss/{n}.git</url>
      <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
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

lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(core, views)
  .dependsOn(core, views)
  .settings(commonSettings)
  .settings(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .settings(commonSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Operationalizing SysSon data matrices as reactive dataflow objects",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucre-expr"      % deps.core.lucre,
      "edu.ucar"      %  "netcdf4"         % deps.core.netCDF,
      "de.sciss"      %% "filecache-txn"   % deps.core.fileCache,
      "de.sciss"      %% "audiofile"       % deps.core.audioFile,
      "de.sciss"      %% "fscape-lucre"    % deps.core.fscape,         // asynchronous processing
      "org.scalatest" %% "scalatest"       % deps.test.scalaTest % Test,
      "de.sciss"      %% "lucre-bdb"       % deps.core.lucre     % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion),
    initialCommands in console :=
      """import de.sciss.lucre.matrix._
        |import Implicits._
        |import de.sciss.lucre.{event => evt}
        |implicit val system = evt.InMemory()
        |type S = evt.InMemory
        |import system.{step => atomic}
        |val z = atomic { implicit tx => Matrix.zeros(13, 21) } // to play around with
        |""".stripMargin
  )

lazy val views = project.withId(s"$baseNameL-views").in(file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name        := s"$baseName-views",
    description := "Swing views for LucreMatrix",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing" % deps.views.lucreSwing,
      "de.sciss" %  "submin"     % deps.test.submin % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-views" % mimaVersion)
  )

