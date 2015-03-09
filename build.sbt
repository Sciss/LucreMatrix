lazy val baseName           = "LucreMatrix"

def baseNameL               = baseName.toLowerCase

lazy val projectVersion     = "0.9.0"

lazy val eventVersion       = "2.7.3"

lazy val netCDFVersion      = "4.3.23"  // be careful: 4.5 will drop Java 6 support

lazy val audioFileVersion   = "1.4.4"

lazy val fileCacheVersion   = "0.3.2"

// ---- core/test ----

lazy val scalaTestVersion   = "2.2.4"

lazy val lucreSTMVersion    = "2.1.1"

// ---- views ----

lazy val lucreSwingVersion  = "0.9.0"

// ---- views/test ----

lazy val webLaFVersion      = "1.28"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := "2.11.6",
  crossScalaVersions := Seq("2.11.6", "2.10.5"),
  homepage           := Some(url("https://github.com/iem-projects/" + baseName)),
  licenses           := Seq("LGPL v2.1+" -> url("https://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture"),
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
  // ---- publishing ----
  publishMavenStyle := true,
  // maven repository for NetCDF library
  resolvers       += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases",
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

lazy val root = Project(
  id            = baseNameL,
  base          = file("."),
  aggregate     = Seq(core, views),
  dependencies  = Seq(core, views),
  settings      = commonSettings ++ Seq(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )
)

lazy val core = Project(
  id            = s"$baseNameL-core",
  base          = file("core"),
  settings      = commonSettings ++ Seq(
    name        := s"$baseName-core",
    description := "Operationalizing SysSon data matrices as reactive dataflow objects",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucreevent"      % eventVersion,
      "edu.ucar"      %  "netcdf"          % netCDFVersion,
      // "de.sciss"      %% "fileutil"        % fileUtilVersion,
      "de.sciss"      %% "filecache-txn"   % fileCacheVersion,
      "de.sciss"      %% "scalaaudiofile"  % audioFileVersion,
      "org.scalatest" %% "scalatest"       % scalaTestVersion % "test",
      "de.sciss"      %% "lucrestm-bdb"    % lucreSTMVersion  % "test"
    ),
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
)

lazy val views = Project(
  id            = s"$baseNameL-views",
  base          = file("views"),
  dependencies  = Seq(core),
  settings      = commonSettings ++ Seq(
    name        := s"$baseName-views",
    description := "Swing views for LucreMatrix",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing" % lucreSwingVersion,
      "de.sciss" %  "weblaf"     % webLaFVersion  % "test"
    )
  )
)
