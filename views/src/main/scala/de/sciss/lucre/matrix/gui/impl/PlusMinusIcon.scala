/*
 *  PlusMinusIcon.scala
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

package de.sciss.lucre.matrix.gui.impl

import java.awt
import java.awt.Graphics
import javax.swing.{UIManager, Icon}

abstract class PlusMinusIcon extends Icon {
  final def getIconHeight = 12
  final def getIconWidth  = 12

  final def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
    g.setColor(UIManager.getColor(if (c.isEnabled) "Label.foreground" else "Label.disabledForeground"))
    paintImpl(g, x, y)
  }

  protected def paintImpl(g: Graphics, x: Int, y: Int): Unit
}

object MinusIcon extends PlusMinusIcon {
  protected def paintImpl(g: Graphics, x: Int, y: Int): Unit =
    g.fillRect(x, y + 6 - 2, 12, 4)
}

object PlusIcon extends PlusMinusIcon {
  protected def paintImpl(g: Graphics, x: Int, y: Int): Unit = {
    g.fillRect(x, y + 6 - 2, 12, 4)
    g.fillRect(x + 6 - 2, y, 4, 12)
  }
}
