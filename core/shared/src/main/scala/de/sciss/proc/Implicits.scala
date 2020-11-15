/*
 *  Implicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.{BooleanObj, Folder, Obj, StringObj, Txn}
import de.sciss.span.SpanLike

import scala.reflect.ClassTag

object Implicits {
  implicit class SecFrames(val `this`: Double) extends AnyVal { me =>
    import me.{`this` => d}

    /** Interprets the number as a duration in seconds, and converts it to sample frames,
      * based on the standard `Timeline` sample-rate.
      */
    def secondsToFrames: Long = (d * TimeRef.SampleRate + 0.5).toLong
  }
  
  implicit class SpanComparisons(val `this`: SpanLike) extends AnyVal {
    import `this`.{compareStart, compareStop}

    def startsBefore(frame: Long): Boolean = compareStart(frame) <  0
    def startsAt    (frame: Long): Boolean = compareStart(frame) == 0
    def startsAfter (frame: Long): Boolean = compareStart(frame) >  0

    def stopsBefore (frame: Long): Boolean = compareStop (frame) <  0
    def stopsAt     (frame: Long): Boolean = compareStop (frame) == 0
    def stopsAfter  (frame: Long): Boolean = compareStop (frame) >  0
  }

  implicit class FolderOps[T <: Txn[T]](val `this`: Folder[T]) extends AnyVal { me =>
    import me.{`this` => folder}

    def / (child: String)(implicit tx: T): Option[Obj[T]] = {
      val res = folder.iterator.filter { obj =>
        obj.name == child
      } .toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in $folder")
      res
    }

    def $ [R[~ <: Txn[~]] <: Obj[~]](child: String)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]] =
      / (child).collect {
        case value if ct.runtimeClass.isAssignableFrom(value.getClass) =>
          value.asInstanceOf[R[T]]
      }

    def ! [R[~ <: Txn[~]] <: Obj[~]](child: String)(implicit tx: T, ct: ClassTag[R[T]]): R[T] =
      $[R](child).getOrElse(throw new NoSuchElementException(
        s"""Folder(${`this`.name}).![${ct.runtimeClass.getName}]("$child")"""))
  }

//  implicit class EnsembleOps[T <: Txn[T]](val `this`: Ensemble[T]) extends AnyVal { me =>
//    import me.{`this` => ensemble}
//
//    def / (child: String)(implicit tx: T): Option[Obj[T]] = {
//      val res = ensemble.folder.iterator.filter { obj =>
//        obj.name == child
//      }.toList.headOption
//
//      // if (res.isEmpty) warn(s"Child $child not found in ${ensemble.attr.name}")
//      res
//    }
//
//    def $ [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: T, ct: ClassTag[R[T]]): Option[R[T]] =
//      / (child).collect {
//        case value if ct.runtimeClass.isAssignableFrom(value.getClass) =>
//          value.asInstanceOf[R[T]]
//      }
//
//    def ! [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: T, ct: ClassTag[R[T]]): R[T] =
//      $[R](child).getOrElse(throw new NoSuchElementException(
//        s"""Ensemble(${`this`.name}).![${ct.runtimeClass.getName}]("$child")"""))
//
//    def play()(implicit tx: T): Unit = play1(value = true )
//    def stop()(implicit tx: T): Unit = play1(value = false)
//
//    private def play1(value: Boolean)(implicit tx: T): Unit = {
//      val BooleanObj.Var(vr) = ensemble.playing
//      val prev = vr()
//      if (!(Expr.isConst(prev) && prev.value == value)) vr() = value
//    }
//
//    def isPlaying(implicit tx: T): Boolean = ensemble.playing.value
//  }

  implicit final class ObjOps[T <: Txn[T]](val `this`: Obj[T]) extends AnyVal { me =>
    import me.{`this` => obj}

    /** Short cut for accessing the attribute `"name"`.
      * If their is no value found, a dummy string `"&lt;unnamed&gt;"` is returned.
      */
    def name(implicit tx: T): String =
      obj.attr.$[StringObj](ObjKeys.attrName).fold("<unnamed>")(_.value)

    /** Short cut for updating the attribute `"name"`. */
    def name_=(value: String)(implicit tx: T): Unit = {
      val valueC  = StringObj.newConst[T](value)
      val attr    = obj.attr
      attr.$[StringObj](ObjKeys.attrName) match {
        case Some(StringObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = StringObj.newVar(valueC)
          attr.put(ObjKeys.attrName, valueVr)
          ()
      }
    }

    /** Short cut for accessing the attribute `"mute"`. */
    def muted(implicit tx: T): Boolean =
      obj.attr.$[BooleanObj](ObjKeys.attrMute).exists(_.value)

    /** Short cut for updating the attribute `"mute"`. */
    def muted_=(value: Boolean)(implicit tx: T): Unit = {
      val valueC  = BooleanObj.newConst[T](value)
      val attr    = obj.attr
      attr.$[BooleanObj](ObjKeys.attrMute) match {
        case Some(BooleanObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = BooleanObj.newVar(valueC)
          attr.put(ObjKeys.attrMute, valueVr)
          ()
      }
    }
  }

  implicit final class ObjAttrMapOps[T <: Txn[T]](val `this`: Obj.AttrMap[T]) extends AnyVal {me =>
    import me.{`this` => attr}

    def ! [R[~ <: Txn[~]] <: Obj[~]](key: String)(implicit tx: T, ct: ClassTag[R[T]]): R[T] =
      attr.$[R](key).getOrElse(throw new NoSuchElementException(
        s"""obj.attr.![${ct.runtimeClass.getName}]("$key")"""))
  }
}