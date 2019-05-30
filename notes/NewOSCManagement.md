# A simple (?) model of assembling OSC messages within a transaction

- client side objects maintain instances of `State` which indicate their state, such as
  a SynthDef being online, a Buffer being allocated or filled with content, a Node running or pausing, etc.
- a `State` carries the following information
  ~ owner and name, for printing purposes
  ~ current value
  ~ bundle index sIdx, indicating the sequence number of the OSC bundle which caused the state to be
    attained on the server side; 0 indicating initial state
  ~ asynchronous boolean sAsync, indicating whether the changing message in the bundle as asynchronous (true)
    or synchronous (false)
- the client side objects provide command methods which issue state changes
- such methods are constructed as follows:
  ~ (1) required conditions are checked, e.g. a buf.cue must check that the allocated-state is true.
    if a condition is not met, a runtime exception is thrown (aborting the transaction)
  ~ required conditions may involve states from other client side objects. for instance synth.play, given
    a list of buffers which are used by the synth, requires that all these buffers be allocated and filled
  ~ (2) `tx.addMessage` is invoked with (a) the corresponding OSC message, (b) the states which are changed
    by this method, (c) the dependencies (list of states)
- tx.addMessage then processes these calls appropriately:
  ~ system maintains index / async of last sent bundle sendIdx / sendAsync;
    where sendAsync indicates whether the client has seen the bundle completed (e.g. `/synced`)
  ~ the dependencies are traversed, and the target bundle index `bndlIdx` / async flag `bndlAsync` are determined.
    begin with `bndlIdx = sendIdx + 1; bndlAsync = false`
    for each changing state `S`, see its bundle index `sIdx` / async flag `sAsync`:
    
```
       if( sIdx > bndlIdx || ((sIdx == bndlIdx) && (sAsync || !bndlAsync)) ) {
          bndlIdx   := sIdx
          bndlAsync := sAsync
       }
```
       
   for each dep. state `D`, see its bundle index `dIdx` / async flag `dAsync`:

```
       if( dIdx > bndlIdx || ((dIdx == bndlIdx) && (dAsync || !bndlAsync)) ) {
          bndlIdx   := dIdx
          bndlAsync := dAsync
       }
```
       
   finally, if the OSC message is asynchronous, adjust `bndlAsync := true`
   
  ~ for each changing state `S`, update `sIdx := bndlIdx`, `sAsync := bndlAsync`
  ~ insert the OSC message into the appropriate bundle structure.
    + a map `m` from bundle index to BundlePrepare is maintained
    + `case class BundlePrepare(direct: Seq[Packet], async: Seq[Packet])`
    + `bndl = m.getOrElse( bndlIdx, new BundlePrepare )`
    + `isDirect = !bndlAsync || !message.isSynchronous`
    + `bndl := if( isDirect ) bndl.copy(direct = bndl.direct :+ message)`
      `               else    bndl.copy(async  = bndl.async  :+ message)`
    + `m += bndlIdx -> bndl`

TODO: 'audible' flag

-----------

scenario: (1) allocate buffer --> (2) read buffer content --> (3) start a synth --> (4) free the synth --> (5) free buffer
(as dependencies) within one transaction.

## version 1 (without smart undo)

(1) BundlePrepare( direct = buf.allocMsg :: Nil, async = Nil ) :: Nil
(2) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = Nil ) :: Nil
(3) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = synth.newMsg :: Nil ) :: Nil
(4) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = synth.newMsg :: synth.freeMsg :: Nil ) :: Nil
(5) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = synth.newMsg :: synth.freeMsg :: Nil ) :: BundlePrepare( direct = buf.freeMsg :: Nil, async :: Nil ) :: Nil

or
(5) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = synth.newMsg :: synth.freeMsg :: buf.freeMsg :: Nil ) :: Nil
?

## version 2 (with smart update)

(1) BundlePrepare( direct = buf.allocMsg :: Nil, async = Nil ) :: Nil
(2) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = Nil ) :: Nil
(3) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = synth.newMsg :: Nil ) :: Nil
(4) BundlePrepare( direct = buf.allocMsg :: buf.readMsg :: Nil, async = Nil ) :: Nil
(5) BundlePrepare( direct = Nil, async = Nil ) :: Nil

bundle preparation:

    a0, ..., ai
    ---
    b0, ..., bj
    ---
    c0, ..., ck
    ---
    d0, ..., dm

assembled as follows:

    [ nil, a0, ..., ai, [ /async, [ nil, b0, ..., bj, [ /async, [ nil, c0, ..., ck, [ /async, [ nil, d0, ..., dm ]]]]]]]

plus an optional step if bundle size becomes too big

::::::::::::::::::::::::::::

(1) it _will_ get easier though with smart folding
(2) on the other hand, it will also get easier with 'throwaway' resources (do not re-use the same buffer or node)
    ; which means in the above example, that (5) is not allowed (buf will treated synth-private and freed using
    an `n_end` listening hook)

::::::::::::::::::::::::::::

elaborate on (2).

first of all, let's review all currently generated messages / states:

--- buffer ---
allocMsg
cueMsg
writeMsg	--> side effect! (if leaveOption == true, we could treat it as foldable)
zeroMsg
closeMsg
freeMsg
(missing: copying messages; they would also constitute side effects)

--- synth ---
newMsg

--- group ---
newMsg

--- node ---
setMsg
mapMsg
moveToHeadMsg, moveToTailMsg, moveBeforeMsg, moveAfterMsg
freeMsg

--- synth-def ---
recvMsg
(freeMsg)	--> currently not implemented

a "throwaway" approach would mean that buffer.free/close, node.free, and synthDef.free make the objects inaccessible for subsequent client operations

note:
- most dependencies are within-the-resource (e.g. true for all buffer operations)
- few are cross-resources (synth.newMsg depending on buffers)

more observations:
- synths are always played straight after instantiation. we can thus simplify the synth resource by not allowing instantiation without automatic playback
- similarly for buffers; we can distinguish cueing for DiskIn and DiskOut, FFT, or 'user buffers' (which may have successive reads / writes, but which
  nevertheless will be immediately allocated)

it thus suffices to maintain only one state, `disposed`, which is initially false and may be only set once to `true`

Buffer.diskIn  --> yields object which can only close
Buffer.diskOut --> ditto.
Buffer.fft     --> ditto.
Buffer.user    --> can read and write, free

...

let's review potential other commands to incorporate:

n_run	- foldable
n_order	- goes beyond single resource, won't be needed and implemented
s_get	- will have side effect; currently not used, but might be needed when jumping in graphemes
b_gen	- copy

-------------

sync  0
async 0
sync  1
async 1
async 1
sync  2
sync  2

 sync -> async +0
 sync ->  sync +0
async -> async +0
async ->  sync +1

basically we need to encode whether a timestamp was produced by an asynchronous message

a simple solution is to allocate the LSB for this

type TimeStamp = Int

where
bit 0         = async?
bits 1 ... 31 = bundle index

-------------

resource management (per server):
   bundle open   stamp
   bundle closed stamp

- the open stamp stores the amount of bundles generated _across transactions_
- when calculating a resource's new time stamp in addMessage, the _minimum value_
  is thus open-stamp minus bundleMap(s).payload.size
- whenever the payload is increased, increase also open-stamp
((- special care needs to be taken of the _one possible_ prepend to payload which
  happens when.... well, when what? is this actually possible? NO))

the case that needs attention:
    txn 1 --> send out bundle with stamp X
    txn n --> 

-------------

`iSortedMap[Int, Bundles]`

flush:

      map.foreach { case (stamp, b) =>
        synchronized {
          if (stamp <= lastSeen) sendOut(stamp, b)
          else await += await.getOrElse(stamp, Vector.empty) :+ b
        }
      }

sendOut(stamp, b):

      if (b.forall(_.isSynchronous) {
        server ! b
        seen(stamp + 1)
      } else {
        server !? (TIMEOUT_MILLIS, b :+ syncMsg, {
          case syncedMsg => seen(stamp + 1)
          case TIMEOUT   => warn(); seen(stamp + 1)
        })
      }

seen(stamp):

      synchronized {
        if (stamp > lastSeen) {
          val from = lastSeen + 1
          lastSeen = stamp
          (from to stamp).foreach { seen =>
            await.get(seen).foreach { b => await -= seen; sendOut(seen, b) }
          }
        }
      }

-----------

Group(s)()
  stamp = 0 (init)
  play --> stamp = 0
  when sending, looks for hasSeen(-1) which is true

a successive transaction will have a bundle index of at least 1 (because txn sent out 1 bundle)

----------

# Notes 190529

We assume sending out two bundles A and B with `<now>` implies that A arrives before B
(guaranteed with TCP in any case)

- define `useLatency = latency > 0`
- traverse the messages in `send`:
  - if `depStamp >= bundleReplySeen`:
    - if synchronous:
      - if `systemTime > 0 && useLatency`
          bundle-time = systemTime + latency
          send out
  
      - if `useLatency`
          bundle-time = `last-sent-time`
          disable late warnings
          send out
   
      - otherwise ("nowish")
          bundle-time = <now>
          send out
    
    - if asynchronous:
      - if `useLatency`
          bundle-time = `last-sent-time`
          disable late warnings
          send out
        
      - otherwise ("nowish")
          bundle-time = <now>
          send out
  
  - otherwise:
     queue
  
    - if synchronous:
    
    - if asynchronous:
     