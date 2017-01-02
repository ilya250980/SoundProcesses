(follow-up discussion from SysSon/FScape caching)

# The need of a generic cached-objects approach

It should be possible to place cached objects for example in attributes of
a proc graph.

```scala
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.synth.proc._
import de.sciss.processor.Processor
import scala.concurrent.Future
import scala.util.Try

trait Gen[S <: Sys[S]] extends Obj[S] {
  def valueType: Obj.Type
  def value      (implicit tx: S#Tx): Option[Try[      Obj[S]]] // analogous to `Future`
  def render()   (implicit tx: S#Tx):        Processor[Obj[S]]
  def rendering  (implicit tx: S#Tx): Option[Processor[Obj[S]]]
  def isRendering(implicit tx: S#Tx): Boolean
}

def test[S <: Sys[S]](implicit tx: S#Tx): Unit = {
  val p = Proc[S]
  val x: Gen[S] = sys.error("TODO")
  p.attr.put("key", x)
}
```

Of course, it would be better if we already had a good strategy for higher-kinded `Obj` types. But we
don't, and we don't have the resources to research on that at the moment. So making a "semi-erased"
type `Gen` with a `valueType` method should be an acceptable compromise (i.e. we can still type-check
relatively early at runtime.)

We could also - perhaps later - tie that to an `OptionObj` or a super-type `OptionLike` which spans
both `Gen` and a hypothetical `OptionObj`. Or `FutureObj`, etc.

If we think of the scenario of FScape, the way that the rendering options are obtained when calling
`validate` is probably that we have a global instance similar to `Mellite.auralSystem`, i.e.
`Mellite.renderingSystem` or something similar. This should be good enough? Perhaps add this to
the `WorkspaceHandle`, or do something we do in the lucre-matrix project with `DataSource.Resolver`?
Or, we simply add an embedded object to `AuralContext` which must be already present in this scenario.

Let's also bear in mind that `Action` execution should also be addressed here, as a general mechanism for
tracking asynchronously running processes (this would speak for embedding the async-runner control
in aural-system).

An `AuralObj` relies on a globally registered factory per object type. It could be the way to register
rendering setups for different objects, e.g. `Action`, `FScape`, ...? That would require that `Gen`
also contains a form of `parent` or `parentType`.

# Creating a `Cached` instance

