# LucreMatrix

Part of research project [SysSon](http://sysson.kug.ac.at/). (C)opyright 2014 Institute of Electronic Music and Acoustics. Written by Hanns Holger Rutz. All rights reserved. Published under the GNU General Public License v2+.

## Building

Builds with sbt 0.13 against Scala 2.11, 2.10.

You need to add the following resolver for the NetCDF library dependency to be found:

    "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

## Linking

Add the following dependency:

    "de.sciss" %% "lucrematrix" % v

The current version `v` is `"0.1.+"`
