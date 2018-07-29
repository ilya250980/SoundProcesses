# Run State

There should be a standard interface for "running" `Obj` instances. This should produce a state which is observable
and can be integrated with views in Mellite, so there can be running stuff without necessarily having a GUI view
corresponding to that, and allowing elements of a workspace to be run "headless". There should be (optional) progress
tracking and the possibility to cancel or stop the process.

## Objects and their Run Actions

|Object                 |Action                     |Type
|-----------------------|---------------------------|-----------
|`Action`               |Execute action             |instant
|`Artifact`             |n/a                        |
|`ArtifactLocation`     |n/a                        |
|`Code`                 |n/a                        |
|`DataSource`\*         |n/a                        |
|`Eye`\*                |?                          |
|`Ensemble`             |Play (aural) ensemble      |indefinite
|`Folder`               |?                          |
|`FadeSpec`             |n/a                        |
|`FScape`               |Render graph               |indefinite/progress
|`FreesoundRetrieval`   |n/a                        |
|`Grapheme`             |Play (aural) grapheme ?    |indefinite
|`Markdown`             |n/a                        |
|`Negatum`\*            |?                          |
|`Nuages`               |?                          |
|`OscNode`\*            |Run receiver               |indefinite
|`ParamSpec`            |n/a                        |
|`Pattern`              |?                          |indefinite
|`Plot`\*               |n/a                        |
|`Proc`                 |Play (aural) proc          |indefinite/progress
|`SOM`\*                |?                          |
|`Sonification`\*       |Play (aural) sonification  |indefinite/progress
|`SphereGNG`\*          |?                          |
|`SVMModel`\*           |?                          |
|`Timeline`             |Play (aural) timeline      |indefinite

\* non-standard-party objects

So we have a hybrid between actions that require an aural context and those that don't. What would be the best
behaviour for containers, like `Folder`, `Ensemble`, `Timeline`; should they require that the server is booted?
Should they gracefully handle both cases? `Ensemble` currently requires the aural context, ensuring that we don't
execute an action before the server is booted, so it will not be out-of-sync with other elements such as `Proc`.
Have a look at timeline editor which controls a transport irrespective of aural system being booted.

-------

Let's call that interface `ObjRunner` and the registry using `ObjRunner.Factory`.

--------

# Comparison online / offline

```
// ---- offline: Rendering ----

object Rendering {
  type State      = GenView.State
  val  Completed  = GenView.Completed
  val  Running    = GenView.Running
  type Running    = GenView.Running

  val  Cancelled = fscape.stream.Cancelled
  type Cancelled = fscape.stream.Cancelled
}
trait Rendering[S <: Sys[S]] extends Observable[S#Tx, Rendering.State] with Disposable[S#Tx] {
  def state(implicit tx: S#Tx): Rendering.State

  def result(implicit tx: S#Tx): Option[Try[Unit]]

  def outputResult(output: OutputGenView[S])(implicit tx: S#Tx): Option[Try[Obj[S]]]

  def control: Control

  /** Like `react` but invokes the function immediately with the current state. */
  def reactNow(fun: S#Tx => Rendering.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def cancel()(implicit tx: S#Tx): Unit
}

// ---- offline: GenView ----

object GenView {
  sealed trait State  
//  case object Stopped extends State
  case object Completed extends State
  case class Running(progress: Double) extends State
}
trait GenView[S <: Sys[S]] extends Observable[S#Tx, GenView.State] with Disposable[S#Tx] {
  def typeId: Int

  def reactNow(fun: S#Tx => GenView.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def state(implicit tx: S#Tx): GenView.State

  def valueType: Obj.Type

  def value(implicit tx: S#Tx): Option[Try[Obj[S]]]
}

// ---- online: AuralView ----

object AuralView {
  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Playing   extends State
}
trait AuralViewBase[S <: Sys[S], -Target] extends Observable[S#Tx, AuralView.State] with Disposable[S#Tx] {
  def state(implicit tx: S#Tx): AuralView.State

  def prepare(timeRef: TimeRef.Option                )(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit

  def stop()(implicit tx: S#Tx): Unit
}
trait AuralView[S <: Sys[S], -Target] extends AuralViewBase[S, Target] {
  def typeId: Int

  def obj: stm.Source[S#Tx, Obj[S]]
}
trait AuralObj[S <: Sys[S]] extends AuralView[S, Unit] {
  def play()(implicit tx: S#Tx): Unit = play(TimeRef.Undefined, ())
}
```

So, `Rendering` and `GenView` assume the process has already been started (there is no stopped state), furthermore,
`GenView` cannot be actively stopped. Before investing energy in fusing these concepts, the best would be to
determine the minimum necessary interface and simply map and wrap the others.

In terms of 'type', assume 'indefinite' as default, and progress reporting can simply "join" the updates if
its available. Progress should already be transactionally decoupled and optionally observed to minimise performance
overhead.

What's the minimum interface:

- allow both for a `self` and an `invoker` (`Action.Universe`)
- `workspace: WorkspaceHandle[S]`
- `cursor: stm.Cursor[S]`
- should we have an `value: Any` as in `Action.Universe`?
- actually, `invoker` and potential `value` should go into `play` and/or `prepare`
- `GenContext` (again for `prepare`)
- realtime `Scheduler`?
- `AuralSystem`?
- prepare, play, stop as actions
- stopped, preparing, prepared, running as state
- when stopped, a "health" status; this could reflect missing inputs, and errors from running
  (should we distinguish between 'stopped' and 'completed'?); perhaps two statuses,
  `inputStatus` and `outputStatus`? Let's begin with a single `status` for simplicity;
  it could encompass a collection with log-level like entries, 'error', 'warn', 'info'
  