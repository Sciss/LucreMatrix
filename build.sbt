name            := "LucreMatrix"

organization    := "de.sciss"   // should probably be at.iem.sysson but this makes Maven Central publishing easier

licenses        := Seq("GPL v2+" -> url("https://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion    := "2.10.3"

// retrieveManaged := true

scalacOptions   := Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq(
  // "de.sciss" %% "lucreevent" % "2.5.+",
  "de.sciss" %% "lucresynth-expr" % "2.2.+",
  "edu.ucar" %  "netcdf"          % "4.3.20"
)
