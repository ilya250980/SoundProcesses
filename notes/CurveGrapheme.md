We should introduce an `Obj`, e.g. `EnvSegment` that can be used for interpolating curve segments.
Since it will most likely be used within a `Grapheme`, this object should be 'punctiform' and not
have an explicit time span associated with it. The object must be a tuple combining a level with
a curve function. The question is whether we denote the _starting_ level or the _target_ level.

Using the _target_ level has a certain intuition; for example `Env` and `IEnv` work that way.
A complication arises for aural-grapheme, however it arises both for starting and target level:
The system must recognise that updating the aural grapheme element player can be triggered by
changing data that lies "ahead" of the current intersection of timeline position and grapheme.

`CurveObj` already exists as a potential use case in `FadeSpec.Obj`. We can probably model
`EnvSegment` in a similar fashion (with both `Apply` and `Const`).

Another open question is whether there is a representation for multi-channel segments? Then we need
a vector of levels and curves. (Or perhaps just support multiple levels).

## AuralView

Perhaps the solution with respect to the different temporal context is to not touch the interfaces
of `AuralView`, `AuralAttribute`, but instead to change the way the factory is found and invoked.
That is to say, we might want to "consume" and replace a preceding scalar attribute by an aural-view
which then encapsulates _both_ the start level and the target level/curve.

`AuralScheduledBase`: this seems to be mostly still valid, few assumptions that would be invalided now.
We will need to pass 'predecessor' object handles around when preparing to create views. It may be that
`ViewID` is useful for that, which currently is `Unit` for grapheme.
Or `mkView` must be changed to allow for the return of views to dispose (probably).

It could also be that we can stick to the 1:1 relationship between model objects and views; such that
scalar starting levels could still have their 'set' command, and the segment would either re-read that
or simply add its curve generator. (I.e. it would always be scheduled with the floor predecessor, if
that's possible).

It could also be that simply in `AuralGraphemeBase`'s `private def elemAdded`, we have to call both
`removeElem` and `addElem`. As far as I can see the `obj: Obj[S]` parameter passed around in `AuralScheduledBase`
is actually opaque, so we could make that a type member that could become an object tuple or whatever
information we need. It appears in exactly the same places as `ViewID` - so we could even collapse the
two (then) opaque types into one.

__The cleanest, i.e. least complicated solution with regard to side-effects, might be to add
`elemReplaced` to `AuralScheduledBase`__. Because if we sequence `elemRemoved` plus `elemAdded`
(or vice versa), we'll end up with other temporary views being unnecessarily prepared/played.

## Creating AuralGraphemeAttribute

Can we short-circuit the view creation, i.e. avoid creating transitory start-level views?
The place would be `processPrepare` in `AuralGraphemeBase`.

Note the asymmetry in the process imagined above: For a scalar start level predecessor we would replace the view,
but for another env-segment predecessor we obviously would not do that. That makes me think we should _keep_ the
scalar view as well. (Does it do any harm?)

------------------

## Rethinking start-level

Just because `Env` organises as `(startLevel, segments*)` does not mean that's the best choice here.
The general notion of grapheme - so far - seems to be that something becomes important _after_ a particular
point in time. From that perspective, but the aural and graphical views would be simpler to conceive if
the type was `EnvSegment(startLevel, curve)`.

That way, we don't need to "consume" and predecessor; we don't have to think about whether we want to
remove or replace the predecessor aural view. Only if a successor is removed, we need to update the
segment aural view, and perhaps even without any involvement of `AuralScheduledBase` other than it changing
it's next scheduler grid.
