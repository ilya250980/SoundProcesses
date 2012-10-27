package de.sciss.synth
package proc
package impl

import de.sciss.osc.Dump
import concurrent.stm.{Ref, Txn}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.{event => evt, DataInput, DataOutput, stm}

object AuralSystemImpl {
   import AuralSystem.Client

   var dumpOSC = false

   def apply[ S <: evt.Sys[ S ]]( implicit tx: S#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] = new Impl[ S ]

//   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
//      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]
//
//   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
//      def write( v: Nothing, out: DataOutput) {}
//      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
//   }

   private final class Impl[ S <: evt.Sys[ S ]]( implicit cursor: stm.Cursor[ S ])
   extends AuralSystem[ S ] {
      impl =>

      override def toString = "AuralSystem@" + hashCode.toHexString

      private val startStopCnt   = Ref( 0 )
      private val clients        = Ref( IIdxSeq.empty[ Client[ S ]])
      private val server         = Ref( Option.empty[ RichServer ])
      private val connection     = Ref( Option.empty[ ServerLike ])

      def start( config: Server.Config )( implicit tx: S#Tx ) : AuralSystem[ S ] = {
         implicit val itx = tx.peer
         val expected = startStopCnt.get + 1
         startStopCnt.set( expected )

         Txn.beforeCommit( _ => {
            if( startStopCnt.get == expected ) doStart( config )
         })( tx.peer )
         this
      }

      def stop()( implicit tx: S#Tx ) : AuralSystem[ S ] = {
         implicit val itx = tx.peer
         val expected = startStopCnt.get + 1
         startStopCnt.set( expected )

         Txn.beforeCommit( _ => {
            if( startStopCnt.get == expected ) doStop()
         })( tx.peer )
         this
      }

      private def doStart( config: Server.Config ) {
         val c = Server.boot( "SoundProcesses", config ) {
            case ServerConnection.Aborted =>
               connection.single() = None

            case ServerConnection.Running( s ) =>
               if( dumpOSC ) s.dumpOSC( Dump.Text )
               cursor.step { implicit tx =>
                  implicit val itx = tx.peer
                  connection() = Some( s )
                  val rich = RichServer( s )
                  server.set( Some( rich ))
                  ProcDemiurg.addServer( rich )( ProcTxn()( tx ))
                  val cs = clients.get
//                  println( "AQUI " + cs )
                  cs.foreach( _.started( rich ))
               }
         }

         Runtime.getRuntime.addShutdownHook( new Thread( new Runnable {
            def run() { impl.shutdown() }
         }))

         connection.single() = Some( c )
      }

      private def shutdown() {
         connection.single().foreach {
            case s: Server => s.quit()
            case _ =>
         }
      }

      private def doStop() {
         connection.single.swap( None ).foreach {
            case c: ServerConnection => c.abort
            case s: Server =>
               cursor.step { implicit tx =>
                  implicit val itx = tx.peer
                  server.get.foreach { rich =>
                     clients.get.foreach( _.stopped() )
                     ProcDemiurg.removeServer( rich )( ProcTxn()( tx ))
                  }
               }
               s.quit()
         }
      }

      def addClient( c: Client[ S ])( implicit tx: S#Tx ) {
         implicit val itx = tx.peer
         clients.transform( _ :+ c )
         val sOpt = server.get
         sOpt.foreach { s =>
            c.started( s )
         }
      }

      def removeClient( c: Client[ S ])( implicit tx: S#Tx ) {
         implicit val itx = tx.peer
         clients.transform { _.filterNot( _ == c )}
      }

      def whenStarted( fun: S#Tx => RichServer => Unit)( implicit tx: S#Tx ) {
         addClient( new Client[ S ] {
            def started( s: RichServer )( implicit tx: S#Tx ) {
               fun( tx )( s )
            }

            def stopped()( implicit tx: S#Tx ) {}
         })
      }
   }
}