/*
 *  ActionRaw.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc
package legacy

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.{Obj, Txn}
import de.sciss.proc
import de.sciss.proc.Code.Example
import de.sciss.serial.DataInput

import scala.annotation.switch
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.Future

/** The old "raw" action is no longer supported.
  * This is merely to be able to open legacy workspaces!
  */
object ActionRaw extends Obj.Type {
  final val typeId = 19

  private final val CONST_EMPTY   = 0
  private final val CONST_JAR     = 1
  private final val CONST_VAR     = 2
  private final val CONST_BODY    = 3

  private lazy val _init: Unit = {
    proc.Code.addType(Code)
  }

  override def init(): Unit = {
    super.init()
    _init
  }

  object Code extends proc.Code.Type {
    final val id = 2

    final val prefix  = "ActionRaw"

    def humanName: String = prefix

    override def examples: ISeq[Example] = Nil

    type Repr = Code

    def docBaseSymbol: String = ""

    def mkCode(source: String): Repr = Code(source)
  }
  final case class Code(source: String) extends proc.Code {
    type In     = String
    type Out    = Unit

    def tpe: proc.Code.Type = Code

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = Future.successful()

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out = ()

    def updateSource(newText: String): Code = copy(source = newText)

    def prelude : String = ""
    def postlude: String = ""
  }

  private def readIdentifiedVar[T <: Txn[T]](in: DataInput, targets: Targets[T])(implicit tx: T): Obj[T] = {
    val id = targets.id
    id.readVar[Obj[T]](in)  // val peer = tx.readVar[ActionRaw[S]](targets.id, in)
    new Tag.Impl[T](id)     // return "any object"
  }

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    in.readByte() match {
      case 0 =>
        val targets = Targets.readIdentified(in)
        in.readByte() match {
          case CONST_VAR =>
            readIdentifiedVar(in, targets)
          case other => sys.error(s"Unexpected action cookie $other")
        }

      case 3 =>
        val id = tx.readId(in)
        (in.readByte(): @switch) match {
          case CONST_JAR =>
            /*val name    =*/ in.readUTF()
            val jarSize = in.readInt()
            val jar     = new Array[Byte](jarSize)
            in.readFully(jar)
            new Tag.Impl[T](id) // return "any object"

          case CONST_BODY   =>
            /*val actionId =*/ in.readUTF()
            new Tag.Impl[T](id) // return "any object"

          case CONST_EMPTY  =>
            new Tag.Impl[T](id) // return "any object"

          case other => sys.error(s"Unexpected action cookie $other")
        }
    }
  }
}
