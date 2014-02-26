name            := "LucreMatrix"

organization    := "de.sciss"   // should probably be at.iem.sysson but this makes Maven Central publishing easier

licenses        := Seq("GPL v2+" -> url("https://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion    := "2.10.3"

// retrieveManaged := true

scalacOptions   := Seq("-deprecation", "-unchecked", "-feature")

initialCommands in console := """import de.sciss.lucre.matrix._
                                |import Implicits._
                                |import de.sciss.lucre.{event => evt}
                                |implicit val system = evt.InMemory()
                                |type S = evt.InMemory
                                |import system.{step => atomic}
                                |val z = atomic { implicit tx => Matrix.zeros(13, 21) } // to play around with
                                |""".stripMargin

libraryDependencies ++= Seq(
  // "de.sciss" %% "lucreevent" % "2.5.+",
  "de.sciss"      %% "lucresynth-expr" % "2.2.+",
  "edu.ucar"      %  "netcdf"          % "4.3.20",
  "org.scalatest" %  "scalatest_2.10"  % "2.0"     % "test",
  "de.sciss"      %% "lucrestm-bdb"    % "2.0.1+"  % "test"
)
