# LucreMatrix

[![Build Status](https://travis-ci.org/iem-projects/LucreMatrix.svg?branch=master)](https://travis-ci.org/item-projects/LucreMatrix)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrematrix_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrematrix_2.11)

Part of research project [SysSon](http://sysson.kug.ac.at/). (C)opyright 2014&ndash;2015 Institute of Electronic Music and Acoustics. (C)opyright 2014&ndash;2015 by Hanns Holger Rutz. All rights reserved. Published under the GNU Lesser General Public License v2.1+.

## Building

Builds with sbt 0.13 against Scala 2.11, 2.10.

You need to add the following resolver for the NetCDF library dependency to be found:

    "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

## Linking

Add the following dependency:

    "de.sciss" %% "lucrematrix" % v

The current stable version `v` is `"0.8.0"`
