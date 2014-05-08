package de.sciss.lucre.matrix
package gui

import scala.swing.{CheckBox, MenuBar, Menu, MenuItem, MainFrame, Frame, SimpleSwingApplication}
import de.sciss.lucre.event.InMemory
import Implicits._
import de.sciss.desktop.impl.UndoManagerImpl
import javax.swing.UIManager
import de.sciss.lucre.swing.View
import scala.util.control.NonFatal

object Demo extends SimpleSwingApplication {
  type S                  = InMemory
  implicit val system: S  = InMemory()

  implicit val undo       = new UndoManagerImpl {
    protected var dirty: Boolean = false
  }

  override def main(args: Array[String]): Unit = {
    //    if (Desktop.isLinux) UIManager.getInstalledLookAndFeels.find(_.getName contains "GTK+").foreach { info =>
    //      UIManager.setLookAndFeel(info.getClassName)
    //    }
    try {
      val web = "com.alee.laf.WebLookAndFeel"
      UIManager.installLookAndFeel("Web Look And Feel", web)
      UIManager.setLookAndFeel(web)
    } catch {
      case NonFatal(_) =>
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
      val m         = MatrixView[S]
      val m0        = Matrix.Var[S](Matrix.newConst2D[S](Vec(Vec(1, 2, 3), Vec(4, 5, 6))))
      m.matrix      = Some(m0)
      m.rowHeaders  = Vec.fill(m0.rank)(View.wrap[S](new CheckBox()))
      m
    }
    new MainFrame {
      title     = "Matrix View"
      contents  = view.component
      menuBar   = mb
      pack()
      //      size = {
      //        val sz    = size
      //        sz.width  = math.max(sz.width , 400)
      //        sz.height = math.max(sz.height, 200)
      //        sz
      //      }

      view.addListener {
        case MatrixView.Resized => pack()
      }

      centerOnScreen()
      open()
    }
  }
}
