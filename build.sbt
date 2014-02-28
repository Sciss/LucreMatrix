lazy val baseName       = "LucreMatrix"

def baseNameL           = baseName.toLowerCase

lazy val projectVersion = "0.1.0-SNAPSHOT"

lazy val eventVersion   = "2.6.+"

lazy val swingVersion   = "0.1.+"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version         := projectVersion,
  organization    := "de.sciss",
  scalaVersion    := "2.10.3",
  homepage        := Some(url("https://github.com/iem-projects/" + baseName)),
  licenses        := Seq("GPL v2+" -> url("https://www.gnu.org/licenses/gpl-2.0.txt")),
  // retrieveManaged := true,
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
  // ---- publishing ----
  publishMavenStyle := true,
  publishTo := {
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
  id            = "root",
  base          = file("."),
  aggregate     = Seq(core, views),
  dependencies  = Seq(core, views),
  settings      = commonSettings ++ Seq(
    packagedArtifacts := Map.empty           // prevent publishing anything!
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
      "edu.ucar"      %  "netcdf"          % "4.3.20",
      "org.scalatest" %  "scalatest_2.10"  % "2.0"     % "test",
      "de.sciss"      %% "lucrestm-bdb"    % "2.0.1+"  % "test"
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
      "de.sciss" %% "lucreswing" % swingVersion
    )
  )
)
