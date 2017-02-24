# LucreMatrix

[![Build Status](https://travis-ci.org/iem-projects/LucreMatrix.svg?branch=master)](https://travis-ci.org/iem-projects/LucreMatrix)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrematrix_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrematrix_2.11)

Part of research project [SysSon](http://sysson.kug.ac.at/). (C)opyright 2014&ndash;2017 Institute of Electronic Music and Acoustics. (C)opyright 2014&ndash;2015 by Hanns Holger Rutz. All rights reserved. Published under the GNU Lesser General Public License v2.1+.

## Building

Builds with sbt 0.13 against Scala 2.12, 2.11, 2.10. We now use NetCDF-4 v4.6.6 which requires Java 7 (Java 6 is no longer supported).

You need to add the following resolver for the NetCDF library dependency to be found:

    "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

## Linking

Add the following dependency:

    "at.iem" %% "lucrematrix" % v

The current stable version `v` is `"1.1.0"`.

The last version based on NetCDF 4.3.22 supporting Java 6 is:

    "de.sciss" %% "lucrematrix" % "0.12.0"
