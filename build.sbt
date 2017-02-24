lazy val baseName           = "LucreMatrix"
lazy val baseNameL          = baseName.toLowerCase

lazy val projectVersion     = "1.1.0"
lazy val mimaVersion        = "1.0.0"

lazy val scalaMainVersion   = "2.12.1"

// ---- core dependencies ----

lazy val netCDFVersion      = "4.6.8"
lazy val audioFileVersion   = "1.4.6"
lazy val fileCacheVersion   = "0.3.4"
lazy val lucreVersion       = "3.3.2"

// ---- core/test dependencies ----

lazy val scalaTestVersion   = "3.0.1"

// ---- views dependencies ----

lazy val lucreSwingVersion  = "1.4.3"

// ---- views/test dependencies ----

lazy val subminVersion      = "0.2.1"

// ----

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "at.iem",
  scalaVersion       := scalaMainVersion,
  crossScalaVersions := Seq(scalaMainVersion, "2.11.8", "2.10.6"),
  homepage           := Some(url(s"https://github.com/iem-projects/$baseName")),
  licenses           := Seq("LGPL v2.1+" -> url("https://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
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
      <url>git@github.com:iem-projects/{n}.git</url>
      <connection>scm:git:git@github.com:iem-projects/{n}.git</connection>
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

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(core, views).
  dependsOn(core, views).
  settings(commonSettings).
  settings(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  settings(commonSettings).
  settings(
    name        := s"$baseName-core",
    description := "Operationalizing SysSon data matrices as reactive dataflow objects",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucre-expr"      % lucreVersion,
      "edu.ucar"      %  "netcdf4"         % netCDFVersion,
      "de.sciss"      %% "filecache-txn"   % fileCacheVersion,
      "de.sciss"      %% "scalaaudiofile"  % audioFileVersion,
      "org.scalatest" %% "scalatest"       % scalaTestVersion % "test",
      "de.sciss"      %% "lucre-bdb"       % lucreVersion     % "test"
    ),
    mimaPreviousArtifacts := Set("at.iem" %% s"$baseNameL-core" % mimaVersion),
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

lazy val views = Project(id = s"$baseNameL-views", base = file("views")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    name        := s"$baseName-views",
    description := "Swing views for LucreMatrix",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing" % lucreSwingVersion,
      "de.sciss" %  "submin"     % subminVersion  % "test"
    ),
    mimaPreviousArtifacts := Set("at.iem" %% s"$baseNameL-views" % mimaVersion)
  )
