/*
 *  MatrixRoot.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package impl

trait MatrixRoot[S <: Sys[S]] extends Matrix[S] {
  final def getDimensionKey(index: Int, useChannels: Boolean)(implicit tx: S#Tx): Matrix.Key = {
    val streamDim = if (useChannels) -1 else 0
    dimensions.apply(index).getKey(streamDim = streamDim)
  }
}
