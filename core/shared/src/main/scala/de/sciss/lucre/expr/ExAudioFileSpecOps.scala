/*
 *  ExAudioFileSpecOps.scala
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

package de.sciss.lucre.expr

import de.sciss.audiofile.{AudioFileSpec => _AudioFileSpec}
import de.sciss.lucre.expr.graph.{AudioFileSpec, Ex}

final class ExAudioFileSpecOps(private val x: Ex[_AudioFileSpec]) extends AnyVal {
  def numChannels : Ex[Int    ] = AudioFileSpec.NumChannels(x)
  def numFrames   : Ex[Long   ] = AudioFileSpec.NumFrames  (x)
  def sampleRate  : Ex[Double ] = AudioFileSpec.SampleRate (x)
}