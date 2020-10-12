# SoundProcesses

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/Sciss/Mellite?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/Sciss/SoundProcesses.svg?branch=main)](https://travis-ci.org/Sciss/SoundProcesses)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/soundprocesses_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/soundprocesses_2.13)

## statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes in the Scala 
programming language. It is (C)opyright 2010&ndash;2020 by Hanns Holger Rutz. All rights reserved. SoundProcesses 
is released under the [GNU Affero General Public License](https://git.iem.at/sciss/SoundProcesses/raw/main/LICENSE) v3+
and comes with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

Further reading:

 - Rutz, H. H., "SoundProcesses: A New Computer Music Framework," in Proceedings of the ICMC|SMC|2014 Joint Conference, Athens 2014.

For tutorials, see the [Mellite website](https://www.sciss.de/mellite/tutorials.html).

## building

SoundProcesses builds with sbt and Scala 2.13, 2.12 (last version to support Scala 2.11 was 3.31.0).
The dependencies should be downloaded automatically from Maven Central repository.

## linking

The following dependency is necessary:

    "de.sciss" %% "soundprocesses" % v

The current version `v` is `"4.0.1"`.

The following sub modules are available:

    "de.sciss" %% "lucre-synth"             % v  // transactional layer for ScalaCollider
    "de.sciss" %% "soundprocesses-core"     % v  // everything but views and compiler
    "de.sciss" %% "soundprocesses-views"    % v  // common swing views
    "de.sciss" %% "soundprocesses-compiler" % v  // compiler integration

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## usage

Project is still sparse, however
there is a graphical front-end [Mellite](https://www.sciss.de/mellite), and
the Mellite website contains [tutorials on SoundProcesses](https://www.sciss.de/mellite/tut_soundprocesses1.html).

## notes

- currently constant `Expr` object do carry 
  an `id` and thus are **not identical to each other** when created repeatedly even with the same 
  peer constant. This was done in order to satisfy `Obj` property, e.g. for any `IntObj` including 
  its constants. A future version may go back to 'cheap' constants which must be explicitly lifted 
  if one wants to access `attr` on them.
