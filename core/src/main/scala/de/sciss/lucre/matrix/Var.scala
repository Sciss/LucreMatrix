/*
 *  Var.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix

import de.sciss.lucre.{stm, event => evt}

trait Var[S <: Sys[S], A] extends stm.Sink[S#Tx, A] with stm.Source[S#Tx, A] with evt.Node[S]