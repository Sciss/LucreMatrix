package de.sciss.lucre.matrix
package gui

import java.awt.datatransfer.Transferable
import java.awt.event.KeyEvent
import java.awt.{FileDialog, Toolkit}
import javax.swing.KeyStroke
import javax.swing.TransferHandler.TransferSupport

import de.sciss.desktop
import de.sciss.desktop.UndoManager
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.submin.Submin
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}
import ucar.nc2.NetcdfFile

import scala.concurrent.ExecutionContext
import scala.swing.{Action, CheckBox, Frame, MainFrame, Menu, MenuBar, MenuItem, SimpleSwingApplication}
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

object Demo extends SimpleSwingApplication {
  type S                                              = InMemory
  implicit val system   : S                           = InMemory()
  implicit val undo     : UndoManager                 = new UndoManagerImpl
  implicit val resolver : DataSource.Resolver.Seq [S] = DataSource.Resolver.empty
  implicit val ws       : WorkspaceHandle         [S] = WorkspaceHandle.Implicits.dummy

  override def main(args: Array[String]): Unit = {
    try {
      Submin.install(true)
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
        val loc = ArtifactLocation.newConst[S](f.parent)
        val art = Artifact(loc, f)
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

  private val th = new MatrixView.TransferHandler[S] {
    def importInt(t: TransferSupport)(implicit tx: S#Tx): Option[IntObj[S]] =
      DragAndDrop.get(t, DragAndDrop.IntObjFlavor).map(_.source.asInstanceOf[stm.Source[S#Tx, IntObj[S]]]())

    def canImportInt(t: TransferSupport): Boolean = t.isDataFlavorSupported(DragAndDrop.IntObjFlavor)

    def exportInt(x: IntObj[S])(implicit tx: S#Tx): Option[Transferable] = {
      val source = tx.newHandle(x)
      val t = DragAndDrop.Transferable(DragAndDrop.IntObjFlavor)(DragAndDrop.IntExprDrag(source))
      Some(t)
    }
  }

  lazy implicit val context: GenContext[S] = system.step { implicit tx => GenContext[S] }

  lazy val view: MatrixView[S] = system.step { implicit tx =>
    import ExecutionContext.Implicits.global
    val m         = MatrixView[S](Some(th))
    val c         = Matrix.newConst2D[S]("M", Vec(Vec(1, 2, 3), Vec(4, 5, 6)))
    val m0        = Matrix.Var[S](c)
    m0.changed.react { implicit tx => u => println(s"Observed: $u") }
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
        contents += new MenuItem(Action("Debug Print") {
          system.step { implicit tx =>
            import ExecutionContext.Implicits.global
            view.matrix.foreach { m =>
              m.debugFlatten.onComplete {
                case Success(vec) => println(vec.mkString("[", ", ", "]"))
                case Failure(ex)  => ex.printStackTrace()
              }
            }
          }
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
