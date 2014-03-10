package de.sciss.lucre.matrix
package gui

import scala.swing.{MenuBar, Menu, MenuItem, MainFrame, Frame, SimpleSwingApplication}
import de.sciss.lucre.event.InMemory
import Implicits._
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.Desktop
import javax.swing.UIManager

object Demo extends SimpleSwingApplication {
  type S                  = InMemory
  implicit val system: S  = InMemory()

  implicit val undo       = new UndoManagerImpl {
    protected var dirty: Boolean = false
  }

  override def main(args: Array[String]): Unit = {
    if (Desktop.isLinux) UIManager.getInstalledLookAndFeels.find(_.getName contains "GTK+").foreach { info =>
      UIManager.setLookAndFeel(info.getClassName)
    }
    super.main(args)
  }

  lazy val top: Frame = {
    val mb = new MenuBar {
      contents += new Menu("Edit") {
        contents += new MenuItem(undo.undoAction)
        contents += new MenuItem(undo.redoAction)
      }
    }

    val view = system.step { implicit tx =>
      val m     = MatrixView[S]
      val m0    = Matrix.Var[S](Matrix.newConst2D[S](Vec(Vec(1, 2, 3), Vec(4, 5, 6))))
      m.matrix  = Some(m0)
      m
    }
    new MainFrame {
      title     = "Matrix View"
      contents  = view.component
      menuBar   = mb
      pack()
      size = {
        val sz = size
        sz.width  = math.max(sz.width , 400)
        sz.height = math.max(sz.height, 200)
        sz
      }
      centerOnScreen()
      open()
    }
  }
}
