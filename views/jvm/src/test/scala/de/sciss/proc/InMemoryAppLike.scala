package de.sciss.proc

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.{Cursor, Workspace => LWorkspace}

trait InMemoryAppLike extends AppLike {
  type S = InMemory
  type T = InMemory.Txn
  implicit val system: S with Cursor[T] = InMemory()

  implicit val undoT: UndoManager[T] = UndoManager()
  implicit lazy val workspace: LWorkspace[T] = Workspace.Implicits.dummy
}
