package de.sciss.lucre.matrix
package gui

import java.awt.{FileDialog, Toolkit}
import java.awt.event.KeyEvent

import de.sciss.file._
import de.sciss.lucre.artifact.ArtifactLocation
import ucar.nc2.NetcdfFile

import scala.concurrent.ExecutionContext
import scala.swing.{Action, CheckBox, MenuBar, Menu, MenuItem, MainFrame, Frame, SimpleSwingApplication}
import de.sciss.lucre.event.InMemory
import Implicits._
import de.sciss.desktop.impl.UndoManagerImpl
import javax.swing.{KeyStroke, UIManager}
import de.sciss.lucre.swing.View
import scala.util.control.NonFatal
import de.sciss.lucre.swing.deferTx
import de.sciss.desktop

object Demo extends SimpleSwingApplication {
  type S                  = InMemory
  implicit val system: S  = InMemory()

  implicit val undo       = new UndoManagerImpl

  implicit val resolver = DataSource.Resolver.empty[S]

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

  private def openNetCDF(): Unit = {
    val dlg = new FileDialog(null: java.awt.Frame)
    dlg.setVisible(true)
    if (dlg.getFile != null) {
      val f   = new File(dlg.getDirectory, dlg.getFile)
      val net = NetcdfFile.open(f.path).setImmutable()
      resolver += net
      val (dsH, names)  = system.step { implicit tx =>
        val loc = ArtifactLocation(f.parent)
        val art = loc.add(f)
        val ds  = DataSource(art)
        val vs  = ds.variables
        (tx.newHandle(ds), vs.map(_.name))
      }
      val nameOpt = if (names.isEmpty) None else if (names.size == 1) names.headOption else {
        val opt = desktop.OptionPane.comboInput(message = "Select Variable", options = names, initial = names.head)
        opt.show()
        // the following line is wrong. Better call Desktop
        //        val res = Dialog.showOptions(message = "Select Variable", entries = names, initial = 0)
        //        if (res.id < 0) None else Some(names(res.id))
      }
      nameOpt.foreach { name =>
        system.step { implicit tx =>
          val ds = dsH()
          val dimOpt = ds.variables.find(_.name == name)
          dimOpt.fold {
            view.matrix = None
          } { dim =>
            view.matrix match {
              case Some(Matrix.Var(vr)) => vr() = dim
              case _ => view.matrix = Some(dim)
            }
          }
          deferTx {
            top.pack()
            // view.component.revalidate()
            // view.component.repaint()
          }
        }
      }
    }
  }

  lazy val view = system.step { implicit tx =>
    import ExecutionContext.Implicits.global
    val m         = MatrixView[S]
    val c         = Matrix.newConst2D[S]("M", Vec(Vec(1, 2, 3), Vec(4, 5, 6)))
    // println(s"C: SHAPE = ${c.shape}, RANGES = ${c.ranges}")
    val m0        = Matrix.Var[S](c)
    // println(s"V: SHAPE = ${m0.shape}, RANGES = ${m0.ranges}")
    m.matrix      = Some(m0)
    m.rowHeaders  = Vec.fill(m0.rank)(View.wrap[S](new CheckBox()))
    m
  }

  lazy val top: Frame = {
    val mb = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Open NetCDF...") {
          accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))
          def apply(): Unit = openNetCDF()
        })
      }
      contents += new Menu("Edit") {
        contents += new MenuItem(undo.undoAction)
        contents += new MenuItem(undo.redoAction)
      }
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
