/*
 *  Implicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.expr.{BooleanObj, Expr, StringObj}
import de.sciss.lucre.stm.{Folder, Obj, Sys}
import de.sciss.span.SpanLike

import scala.language.higherKinds
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

  implicit class FolderOps[S <: Sys[S]](val `this`: Folder[S]) extends AnyVal { me =>
    import me.{`this` => folder}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = folder.iterator.filter { obj =>
        obj.name == child
      } .toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in $folder")
      res
    }

    def $ [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]] =
      / (child).collect {
        case value if ct.runtimeClass.isAssignableFrom(value.getClass) =>
          value.asInstanceOf[R[S]]
      }

    def ! [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] =
      $[R](child).getOrElse(throw new NoSuchElementException(
        s"""Folder(${`this`.name}).![${ct.runtimeClass.getName}]("$child")"""))
  }

  implicit class EnsembleOps[S <: Sys[S]](val `this`: Ensemble[S]) extends AnyVal { me =>
    import me.{`this` => ensemble}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = ensemble.folder.iterator.filter { obj =>
        obj.name == child
      }.toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in ${ensemble.attr.name}")
      res
    }

    def $ [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]] =
      / (child).collect {
        case value if ct.runtimeClass.isAssignableFrom(value.getClass) =>
          value.asInstanceOf[R[S]]
      }

    def ! [R[~ <: Sys[~]] <: Obj[~]](child: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] =
      $[R](child).getOrElse(throw new NoSuchElementException(
        s"""Ensemble(${`this`.name}).![${ct.runtimeClass.getName}]("$child")"""))

    def play()(implicit tx: S#Tx): Unit = play1(value = true )
    def stop()(implicit tx: S#Tx): Unit = play1(value = false)

    private def play1(value: Boolean)(implicit tx: S#Tx): Unit = {
      val BooleanObj.Var(vr) = ensemble.playing
      val prev = vr()
      if (!(Expr.isConst(prev) && prev.value == value)) vr() = value
    }

    def isPlaying(implicit tx: S#Tx): Boolean = ensemble.playing.value
  }

  implicit final class ObjOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal { me =>
    import me.{`this` => obj}

    /** Short cut for accessing the attribute `"name"`.
      * If their is no value found, a dummy string `"&lt;unnamed&gt;"` is returned.
      */
    def name(implicit tx: S#Tx): String =
      obj.attr.$[StringObj](ObjKeys.attrName).fold("<unnamed>")(_.value)

    /** Short cut for updating the attribute `"name"`. */
    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      val valueC  = StringObj.newConst[S](value)
      val attr    = obj.attr
      attr.$[StringObj](ObjKeys.attrName) match {
        case Some(StringObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = StringObj.newVar(valueC)
          attr.put(ObjKeys.attrName, valueVr)
      }
    }

    /** Short cut for accessing the attribute `"mute"`. */
    def muted(implicit tx: S#Tx): Boolean =
      obj.attr.$[BooleanObj](ObjKeys.attrMute).exists(_.value)

    /** Short cut for updating the attribute `"mute"`. */
    def muted_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      val valueC  = BooleanObj.newConst[S](value)
      val attr    = obj.attr
      attr.$[BooleanObj](ObjKeys.attrMute) match {
        case Some(BooleanObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = BooleanObj.newVar(valueC)
          attr.put(ObjKeys.attrMute, valueVr)
      }
    }
  }

  implicit final class ObjAttrMapOps[S <: Sys[S]](val `this`: Obj.AttrMap[S]) extends AnyVal {me =>
    import me.{`this` => attr}

    def ! [R[~ <: Sys[~]] <: Obj[~]](key: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] =
      attr.$[R](key).getOrElse(throw new NoSuchElementException(
        s"""obj.attr.![${ct.runtimeClass.getName}]("$key")"""))
  }
}