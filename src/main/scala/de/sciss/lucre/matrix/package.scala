/*
 *  package.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre

package object matrix {
  type Sys[S <: Sys[S]] = de.sciss.lucre.event.Sys[S]

  val Longs     = de.sciss.lucre.synth.expr.Longs
  val Strings   = de.sciss.lucre.synth.expr.Strings
  val Ints      = de.sciss.lucre.synth.expr.Ints

  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq
}