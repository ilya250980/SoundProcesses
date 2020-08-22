package de.sciss.synth.proc

import de.sciss.ConfluentEventSpec
import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleVector, BooleanObj, DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.synth.Curve
import de.sciss.synth.io.AudioFileSpec

/*
  To run only this suite:

  testOnly de.sciss.synth.proc.AttributesSpec

  */
class AttributesSpec extends ConfluentEventSpec {
  // import imp._
  // import ExprImplicits._

  "Attrs" should "serialize and de-serialize" in { system =>
    val pH = system.step { implicit tx =>
      val p     = Proc[S]()
      val obj   = p // Obj(peer)
      obj.attr.put("foo", IntObj.newVar[S](1234))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p     = pH()
      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
      val expr  = p.attr.$[IntObj]("foo")
      val v     = expr match {
        case Some(IntObj.Var(vr)) => vr().value
        case _ => -1
      }
      assert(v == 1234, s"Did not find an Expr.Var: $expr")
    }

    // ---- now for other types ----

    val fade = FadeSpec(1234L, Curve.parametric(7.8f), 0.56f)
    val spec = AudioFileSpec(numChannels = 3, numFrames = 45L, sampleRate = 678.9)

    val pgH = system.step { implicit tx =>
      val p     = pH()
      val attr  = p.attr
      attr.put("int"    , IntObj.newConst[S](5678 ))
      val d = DoubleObj.newConst[S](123.4)
      attr.put("double" , d)
      val n = LongObj.newConst[S](1234L)
      attr.put("long"   , n)
      attr.put("boolean", BooleanObj.newConst[S](true ))
      attr.put("string" , StringObj .newConst[S]("123"))

      attr.put("fade"   , FadeSpec.Obj.newConst(fade))
      attr.put("d-vec"  , DoubleVector.newConst(Vector(1.2, 3.4, 5.6)))
      val loc = ArtifactLocation.newConst[S](file("foo"))
      val art = Artifact(loc, Artifact.Child("bar")) // loc.add(file("foo") / "bar")
      attr.put("audio"  , AudioCue.Obj(art, spec, offset = n, gain = d))
      attr.put("loc",     loc)
      val group = Timeline[S]()
      attr.put("group",   group)
      // implicit val groupSer = ProcGroup.Modifiable.serializer[S]
      tx.newHandle(group)
    }

    system.step { implicit tx =>
      val p     = pH()
      val group = pgH()
      val attr  = p.attr
      assert(attr.$[IntObj      ]("int"    ).map(_.value) === Some(5678))
      assert(attr.$[DoubleObj   ]("double" ).map(_.value) === Some(123.4))
      assert(attr.$[LongObj     ]("long"   ).map(_.value) === Some(1234L))
      assert(attr.$[BooleanObj  ]("boolean").map(_.value) === Some(true))
      assert(attr.$[StringObj   ]("string" ).map(_.value) === Some("123"))
      assert(attr.$[FadeSpec.Obj]("fade"   ).map(_.value) === Some(fade))
      assert(attr.$[DoubleVector]("d-vec"  ).map(_.value) === Some(Vector(1.2, 3.4, 5.6)))
      assert(attr.$[Timeline    ]("group"  ) === Some(group))
      assert(attr.$[AudioCue.Obj]("audio").map(_.value) ===
        Some(AudioCue(file("foo") / "bar", spec, 1234L, 123.4)))
      assert(attr.$[ArtifactLocation]("loc").map(_.directory) === Some(file("foo")))
    }
  }
}
