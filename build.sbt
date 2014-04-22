lazy val baseName          = "LucreMatrix"

def baseNameL              = baseName.toLowerCase

lazy val projectVersion    = "0.1.0"

lazy val eventVersion      = "2.6.1"

lazy val netCDFVersion     = "4.3.21"  // be careful: 4.5 will drop Java 6 support

lazy val fileUtilVersion   = "1.1.1"

// ---- core/test ----

lazy val scalaTestVersion  = "2.1.3"

lazy val lucreSTMVersion   = "2.0.4"

// --- views ----

lazy val lucreSwingVersion = "0.2.+"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := "2.11.0",
  crossScalaVersions := Seq("2.11.0", "2.10.4"),
  homepage           := Some(url("https://github.com/iem-projects/" + baseName)),
  licenses           := Seq("GPL v2+" -> url("https://www.gnu.org/licenses/gpl-2.0.txt")),
  retrieveManaged    := true,
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture"),
  // ---- publishing ----
  publishMavenStyle := true,
  // maven repository for NetCDF library
  resolvers       += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases",
  publishTo       := {
    Some(if (version.value endsWith "-SNAPSHOT")
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
      "de.sciss"      %% "fileutil"        % fileUtilVersion,
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
      "de.sciss" %% "lucreswing" % lucreSwingVersion
    )
  )
)
