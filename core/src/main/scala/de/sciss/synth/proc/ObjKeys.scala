/*
 *  ObjKeys.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

object ObjKeys {
  /** Name attribute. Type `String` */
  final val attrName    = "name"
  /** Bus attribute. Type `Int` */
  final val attrBus     = "bus"
  /** Gain factor. Type `Double` */
  final val attrGain    = "gain"
  /** Gain factor. Type `Boolean` */
  final val attrMute    = "mute"
  /** Fade in. Type `FadeSpec` */
  final val attrFadeIn  = "fade-in"
  /** Fade out. Type `FadeSpec` */
  final val attrFadeOut = "fade-out"
}