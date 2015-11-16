lazy val baseName           = "LucreMatrix"
lazy val baseNameL          = baseName.toLowerCase

lazy val projectVersion     = "0.11.1-SNAPSHOT"

lazy val netCDFVersion      = "4.3.23"  // be careful: 4.5 will drop Java 6 support
lazy val audioFileVersion   = "1.4.5"
lazy val fileCacheVersion   = "0.3.3"

// ---- core/test ----

lazy val scalaTestVersion   = "2.2.5"
lazy val lucreVersion       = "3.2.2"

// ---- views ----

lazy val lucreSwingVersion  = "1.2.2"

// ---- views/test ----

lazy val xstreamVersion     = "1.4.8"   // Maven Central sha1 corruption in previous version
lazy val webLaFVersion      = "1.28"

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := "2.11.7",
  crossScalaVersions := Seq("2.11.7", "2.10.6"),
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
      "edu.ucar"      %  "netcdf"          % netCDFVersion,
      "de.sciss"      %% "filecache-txn"   % fileCacheVersion,
      "de.sciss"      %% "scalaaudiofile"  % audioFileVersion,
      "org.scalatest" %% "scalatest"       % scalaTestVersion % "test",
      "de.sciss"      %% "lucre-bdb"       % lucreVersion     % "test"
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

lazy val views = Project(id = s"$baseNameL-views", base = file("views")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    name        := s"$baseName-views",
    description := "Swing views for LucreMatrix",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing" % lucreSwingVersion,
      "com.thoughtworks.xstream" % "xstream" % xstreamVersion % "test",  // bug in Maven Central
      "de.sciss" %  "weblaf"     % webLaFVersion  % "test"
    )
  )
