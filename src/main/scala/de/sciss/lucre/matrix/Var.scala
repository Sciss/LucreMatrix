package de.sciss.lucre.matrix

import de.sciss.lucre.stm

trait Var[S <: Sys[S], A] extends stm.Sink[S, A] with stm.Source[S, A]