package de.sciss.lucre.matrix

import de.sciss.lucre.{stm, event => evt}

trait Var[S <: Sys[S], A] extends stm.Sink[S#Tx, A] with stm.Source[S#Tx, A] with evt.Node[S]