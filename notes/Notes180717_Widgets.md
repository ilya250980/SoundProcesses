# A hypothetical program

```
val sepWave         = TitledSeparator("Waveform I/O")
val lbIn            = Label("Input file:")
val ggIn            = PathField()
ggIn.mode           = PathField.Input
ggIn.info           = true
val lbOut           = Label("Output file:")
val ggOut           = PathField()
ggOut.mode          = PathField.Output
val ggFileType      = ComboBox()
val ggSmpFmt        = ComboBox()
val ggGain          = NumberField()
ggGain.spec         = ParamSpec(...)
ggGain.value        = -0.20
ggGain.unit         = "dB"
val ggGainType      = ComboBox()
val sepSRC          = TitledSeparator("Sample Rate Conversion")
val lbNewRate       = Label("New rate:")
val ggNewRate       = NumberField()
val ggChangePch     = CheckBox("Change Pitch/Speed")
val lbFltLen        = Label("FIR length:")
val ggFltLen        = ComboBox()
val ggProg          = ProgressBar()
val ggCancel        = Button("X")
ggCancel.enabled    = false
val ggRender        = Button("Render")
ggRender.action     = "render".attr

GroupPanel(...)

```

# On expressions, patterns, and graph elements

Both `Expr` and `Pat` are essentially pull based, but `Expr` does have a push support and observability.
We could, of course, introduce a `Widget <: Obj` and then compose them using attribute maps etc. A `Slider <: Widget`
could then even be an `Expr[Double]`, an `Expr.Var[Double]` or something like that. But we would have to construct
UIs then through dedicated graphical editors—which might be nice, but the work necessary to build the editors
exceeds my current resources for this. Thus, a "graph" and "graph element" based program in a DSL is more viable
(and I was never that interested in graphical UI editors).

There are view-only and editor fields in a widget. Technically, we do not need to be able to treat, for example,
`enabled` as an expression like object that can be plugged into other expressions, as it will only be
programmatically adjusted, and so it would suffice to reuse the expression _assigned to it_ in other places.

So

```
trait Slider {
  def enabled: Sink[Boolean]
  
  def min: Sink[Int]
  def max: Sink[Int]

  def value: Expr.Var[Int]
}

trait Sink[A] {
  def update(x: Expr[A]): Unit
}

trait Example {
  def a: Slider
  def b: Slider
  
  a.min() = 0       // lifted to `Constant`
  a.max() = 100     // lifted to `Constant`
  a.enabled() = b.value sig_== 0
```

versus more 'declarative' style:

```
case class Slider(enabled: Expr[Boolean] = true, min: Expr[Int] = 0, max: Expr[Int] = 100, 
                  value0: Expr[Int] = 0)
  extends GE with Expr[Int]
```

The first approach is better for preserving the ability to serialise across API revisions. It makes it
more difficult to collect the resulting graph, although we could use a case-class in the background and
replace the instances during the build process. (that means we don't have isomorphism between AST and 
written source code, though)

# A note on LucreSwing

Yes, there is functional duplication. All this should be part of Lucre-Swing and replace the clunky `View` things that
use `CellView` in some places. But since SP depends on LS and not vice versa, we need to separate the `Obj`
wrapper to be part of SP or Mllt instead.

So eventually I could build Mllt views from the new widget graph thing altogether. Minus `Action` invocations,
I guess? Bit of dogfooding...

--------------------------

# On expressions, still

If we are going to "duplicate" the `Expr` functionality inside an immutable graph / AST; would it not make
sense to _reduce_ the `Expr` API (e.g. remove the unary and binary operators?) and instead allow us to encapsulate
graphs as an `Expr` `Obj` instead? That would allow us to compose more complex expressions (as a graph) without
the annoying multiple deserialisation and event propagation.

(We could also keep unary/binary expr ops, as they are reasonably small additions; while things like
`StringObj.length -> IntObj` are annoying)

Say that `ExprGraph[A] <: Expr[A]` for example. We could probably use the existing extension-registration mechanism
to add that to lucre-core.

Then a "program" that creates an int expression of the length of a string expression would probably formally have
a 'source' shape instead of a 'fan' shape, i.e. getting its input informally through its attribute map?

```
val s: StringObj[S] = ???
val l: IntObj[S] = ExGraph {
  val _s: Ex[String] = "in".attr[String]
  _s.length: Ex[Int]
}
```

Where there is a type class for the `ExGraph[Int]` to `IntObj` lifting. We could then introduce syntactic sugar
to minimise boiler plate, like

```
val s: StringObj[S] = ???
val l: IntObj[S] = s.map(_.length)
```

The implementation problem is still the propagation from `s.changed` into the `ExGraph`, although that's solvable
in the graph builder, like we're collecting controls in a synth-graph, and so in the transition to `IntObj`, that
thing needs to add its event listeners. (Can we easily abstract over arity here? Probably?)

# Back to widgets

So a widget (container) could work like `ExGraph` in terms of the builder, but the result would not be an `Ex`, and
the graph would separate out the widget elements. Or is there a benefit from having an `Ex` leaf? No?

```
trait Widget[S :< Sys[S]] extends Obj[S] {
  def graph: stm.Ref[S#Tx, Widget.Graph]
}
```

Does `graph` have to be another `Expr` type wrapper as in the case of `Proc`? Probably not? The only reason I can
think of now is to have pre-determined types of graphs (as we have "cheap" graphs for standard `Proc` types).
This should not apply here?

What about the name, `Widget`? That does not reflect the container kind of structure which would be mostly used.
Other names are ambiguous, i.e. `Container`, `Component`, `Gadget`, `Panel`, ... although perhaps `Panel` is more
useful as the _view_ of the object will always have a top-level panel, even if the object designates a single
widget such as a button? How about `View` or `UI`? Acronyms are not nice. `Interface` again ambiguous without the
qualification as `GraphicalInterface`, etc.; `Display`... Let's stick to `Widget` for now. It's still the most
precise and unambiguous; also it reflects that the `Widget.Graph` always has one leaf in the end.

## Implementation of 'mutable' style

Given this:

```
  val s = Slider()
  val v = "key".attr[Double]
  s.value() = v
  val lb = Label("Value:")
  val res = FlowPanel(lb, s)
  res
```

What would the AST / graph look like? We could use a stringly typed or otherwise less type-safe representation:

```
Seq(
  Assign(Slider(id = 0), "value", Attr[Double]("key")),
  Assign(Label(id = 1), "text", Constant("Value:")),
  FlowPanel(Slider(id = 0), Label(id = 1))
)
```

So a bit like `PropertyChangeListener` or beans.

Since we never want to collapse structurally identical elements into one, we can just use the identifiers:

```
Seq(
  Assign(Slider(id = 0), "value", Attr[Double]("key")),
  Assign(Label(id = 1), "text", Constant("Value:")),
  FlowPanel(0, 1)
)
```

We might do away with Scala-Swing's `contents` operations, so instead of

```
  new FlowPanel {
    contents ++= Seq(a, b)
  }
  
  new BoxPanel(Orientation.Vertical) {
    contents += a
    contents += b
  }
```

We could require that contents are always specified in the constructor:

```
  FlowPanel(a, b)
  
  VBoxPanel(a, b)
```

The most complex case will be the equivalent of `GroupPanel`.

We could also just have a separate "property map" in the graph:

```
  Graph(
    elems = Seq(Slider(), Label(), FlowPanel()),
    properties = Map(0 -> Map("value" -> Attr[Double]("key")), 1 -> Map("text" -> Constant("Value:")),
      2 -> Map("contents" -> Seq(0, 1)))
  )
```

That would be pretty insensitive to API evolution.

Back to the "patched" example:

```
  Graph {
    val a = Slider()
    val b = Slider()
  
    a.min()     = 0
    a.max()     = 100
    a.enabled() = b.value sig_== 0
  }
  
  Graph(
    elems = Seq(Slider(), Slider()),
    properties = Map(0 -> Map("min" -> Constant(0), "max" -> Constant(100),
      "enabled" -> BinOp(Sig_==, ???, Constant(0)))
  )
```

Should we rely on thread-local builder and remove `apply`/`update` type of fields, like so:

```
  Graph {
    val a = Slider()
    val b = Slider()
  
    a.min       = 0
    a.max       = 100
    a.enabled   = b.value sig_== 0
  }
```

? Further questions:

- what is `???` here?
- can we avoid `Map[Int, Map[String, Any]]` for `properties`, 
  and have `Map[Int, Map[String, GE]]` instead?

The `???` might be `Property[Double](id = 0, key = "value")`.

# Patterns, again

Should we have a bridge? Where would it be useful? It seems that the plain `Pat` would be "identical" to
the hypothetical `Ex`. We could rename `expand` to `expandStream`, and simply add `expandExpr` for the
expression-like model. The question is if we gain anything by this, because we don't even get lots of
"DRY", as `expandStream` and `expandExpr` would mostly need entirely different representations. (Just as with
FScape's streams). The advantage would be in refactoring, where we would not have the risk of diverging APIs.

The interesting question is how this would play out for real collections, i.e. the case where we have
`Ex <-> SingletonPat` is more trivial. For instance:

```
case class Sorted[A](in: Pat[A])(implicit val ord: ScalarOrd[A]) extends Pattern[A]
```

A UI example for 'sorted':

```
  val l = ListView()
  val s = ListView()
  s.items() = l.items().sorted
```

It's difficult to imagine this with `items` producing an `Ex[A]` instead of `Ex[Seq[S]]`...? Unless we follow
the patterns style and have

```
  trait Ex[A] {
    def value: A
    def values: Seq[A]
  }
```

Such that `values` would return a single element sequence for singleton expressions, and `value` would return
`head` for collection expressions. (What about empty sequences?)

__It seems like a mismatch to try to equalise patterns and expressions__

What __would__ make sense to share is the type classes (`ScalarOrd` etc.) and the `Aux` type of serialisation,
especially as we might also use them in FScape.

# Contents

So `contents` is special somehow in that it is static and does not use expressions. We could thus add a distinct
way to represent them in the graph, and so `properties` could indeed have a more safe value type. Although perhaps
there are other static properties? Say `style` in order to customise the look of a widget similar to what WebLaF
is doing:

```
   val s = Slider()
   s.style = "plain"
```

So the trade-off is homogeneity (wrapping this in `Constant(String)`) versus simplicity (not needing to observe
that expression). Also, there may be more of the non-expression properties, as we consider supporting integrated
views for other types of `Obj`. That could even be opaque, like `ObjView("key")`.

# Widgets

Top-level: `Component`

```
trait Component extends Widget {
  var enabled: Ex[Boolean]
  var tooltip: Ex[String]
  
  // var focusable: Ex[Boolean] etc.
}
```

## Slider

```
trait Slider extends Component {
  var min               : Ex[Int]
  var max               : Ex[Int]
  var value             : Ex[Int]
  var extent            : Ex[Int]

  var paintLabels       : Ex[Boolean]
  var paintTicks        : Ex[Boolean]
  var snapToTicks       : Ex[Boolean]

  var minorTickSpacing  : Ex[Int]
  var majorTickSpacing  : Ex[Int]
  
  var labels: Map[Int, Label]  // ???
}
```

We could even have a "filtered" `def valueNonAdjusting: Ex[Int]` if we want to put that into other expressions?

## ComboBox

```
trait ComboBox[A] extends Component {
  var prototypeDisplayValue: Ex[Option[A]]
  
  var editable: Ex[Boolean]
  
  var items: Ex[Seq[A]]
  
  var selection: Ex[Option[A]] // ???
}
```

## ListView

```
trait ComboBox[A] extends Component {
  var editable: Ex[Boolean]
  
  var items: Ex[Seq[A]]
  
  var selection: Ex[Seq[A]] // ???
}
```

The potentially confusing thing here is `selection`. Since we can only set it declaratively not imperatively,
that would mean the user interaction is suspended when doing so. There _is_ a `setSelectionModel` on `JList`, but
it is always mutable. So we might need a more complex _event_ type of connection, e.g. something that could invoke
`clearSelection`, a _trigger_, such as `Changed(itemsEx)`, or `button.clicked`, making that more versatile than
coupling to an `Action` attribute map entry. We'll end up with a full FRP thing. And that, in turn, could have
consequences for the entire architecture of SP in the long run...

# Triggers

```
trait Button extends Component {
  def clicked: Trigger
}

trait Ex[A] {
  def changed: Trigger  // probably need different name, as `changed: Event` will be used in `Expr`
}

trait Trigger {
  def | (that: Trigger): Trigger
  def & (that: Trigger): Trigger
  
  def ~> (sink: TriggerInput): Unit
}

trait TriggerInput {
  def ++ (that: TriggerInput): TriggerInput  // useful?
}

trait ListView[A] extends Component {
  object selection {
    def indices: Ex[Seq[Int]]
    def values: Ex[Seq[A]]
    
    def clear: TriggerInput
  }
}

trait Example {
  val b = Button()
  b.text = "Clear"
  val l = ListView[String]()
  l.items = Seq("foo", "bar")
  b.clicked ~> l.selection.clear
  
  val c = Button()
  c.text = "Render"
  c.clicked ~> Action("attr-key") // or just "attr-key".attr ?
}
```

Thinking this further:

```
c.clicked ~> Obj("attr-key").play  // or .run ? or .view, ...
```

without the need to insert an extra action to accomplish this. (Basically avoiding having to write `Action`
bodies as much as possible)

What about parametric actions, say selecting a list item or a tab page? Could we combine triggers and expressions
here?

```
c.clicked ~> tabbedPane1.selection.index_=(0)
c.clicked ~> tabbedPane2.selection.index_=(combo.selection.index)

object TabbedPane {
  trait Selection {
    def index: Ex[Int]
    def index_=(value: Ex[Int]): TriggerInput
  }
}
trait TabbedPane {
  def selection: TabbedPane.Selection
}
```

We could use this for initial settings in interactive properties?

```
  val t = TabbedPane()
  // ...
  
  t.selection.index = 0
```

This could create an "unconnected" TriggerInput which might be tracked and stored for initial evaluation?
(In Dotty, this will become nicer and safer as we have implicit functions)

## Events

Could we need something more complex than `c.clicked: Trigger`, i.e. something that gives us, for example,
information on click count, modifiers, ...?

```
trait Clicked extends Trigger {
  def count: Ex[Int]
  def modifiers: Ex[Int]
  def shiftPressed: Ex[Boolean] // etc.
}

trait Example {
  def b: Button
  def t: TabbedPane
  
  val c = b.clicked
  c ~> (t.selection.index = If (c.shiftPressed) 0 Else 1)   // hypothetical syntax
}
```

versus

```
trait Trigger[A] {
  def value: A
  def ~> (sink: TriggerInput): Unit
}

trait Button {
  def clicked: Trigger[(Int, Int)]  // ugly?
}
```
