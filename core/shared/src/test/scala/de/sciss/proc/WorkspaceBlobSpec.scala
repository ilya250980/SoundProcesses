package de.sciss.proc

import de.sciss.proc.Implicits.ObjOps
import org.scalatest.funspec.AnyFunSpec

/*
  To run only this suite:

  testOnly de.sciss.proc.WorkspaceBlobSpec

 */
class WorkspaceBlobSpec extends AnyFunSpec {
  SoundProcesses.init()

  describe("Workspace.Blob") {
    it("should correctly encode to and decode from byte array") {
      type T    = Durable.Txn
      val meta0 = Map("foo" -> "bar", "baz" -> "quux")
      val meta1 = Map("baz" -> "xuuq")
      val meta2 = meta0 ++ meta1
      val name0 = "a tag"
      val ws0   = Workspace.Blob.empty(meta = meta0)
      val arr   = ws0.cursor.step { implicit tx =>
        val tag   = Tag[T]()
        tag.name  = name0
        ws0.root.addLast(tag)
        ws0.toByteArray
      }
      ws0.close()
      val ws1   = Workspace.Blob.fromByteArray(arr, meta = meta1)
      assert(ws1.meta == meta2)
      val (ok, name1) = ws1.cursor.step { implicit tx =>
        val tagOpt = ws1.root.iterator.collectFirst {
          case t: Tag[T] => t
        }
        tagOpt match {
          case Some(tag)  => (true, tag.name)
          case None       => (false, "?")
        }
      }
      ws1.close()
      assert (ok)
      assert (name1 == name0)
    }
  }
}