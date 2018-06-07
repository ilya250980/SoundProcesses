# SoundProcesses

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/Sciss/Mellite?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/Sciss/SoundProcesses.svg?branch=master)](https://travis-ci.org/Sciss/SoundProcesses)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/soundprocesses_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/soundprocesses_2.11)

## statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes in the Scala 
programming language. It is (C)opyright 2010&ndash;2018 by Hanns Holger Rutz. All rights reserved. SoundProcesses 
is released under the [GNU Lesser General Public License](http://github.com/Sciss/SoundProcesses/blob/master/licenses/SoundProcesses-License.txt) v2.1+
and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

Further reading:

 - Rutz, H. H., "SoundProcesses: A New Computer Music Framework," in Proceedings of the ICMC|SMC|2014 Joint Conference, Athens 2014.

## building

SoundProcesses builds with sbt and Scala 2.12, 2.11. The dependencies should be downloaded automatically from maven central repository.

## linking

The following dependency is necessary:

    "de.sciss" %% "soundprocesses" % v

The current version `v` is `"3.20.2"`.

The following sub modules are available:

    "de.sciss" %% "lucrebitemp"     % v          // bi-temporal data structures
    "de.sciss" %% "lucresynth"      % v          // transactional layer for ScalaCollider
    "de.sciss" %% "lucresynth-expr" % v          // standard expression types
    "de.sciss" %% "soundprocesses-core"     % v  // everything but views and compiler
    "de.sciss" %% "soundprocesses-views"    % v  // common swing views
    "de.sciss" %% "soundprocesses-compiler" % v  // compiler integration

For compiling the tests, the following additional resolver may be needed:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## usage

Project is still experimental, and documentation is still sparse.
There is a graphical front-end [Mellite](https://github.com/Sciss/Mellite) (also experimental)...
The Mellite website contains a [tutorial on SoundProcesses](http://sciss.github.io/Mellite/tut_soundprocesses1.html).

## notes

- in the Lucre-3 based version 3 (experimental) branch, currently constant `Expr` object do carry 
  an `id` and thus are **not identical to each other** when created repeatedly even with the same 
  peer constant. This was done in order to satisfy `Obj` property, e.g. for any `IntObj` including 
  its constants. A future version may go back to 'cheap' constants which must be explicitly lifted 
  if one wants to access `attr` on them.
