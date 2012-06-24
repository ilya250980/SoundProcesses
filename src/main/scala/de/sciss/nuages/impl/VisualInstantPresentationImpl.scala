/*
 *  VisualInstantPresentationImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.nuages
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import de.sciss.synth.proc.{Proc, Transport}
import java.awt.{RenderingHints, Graphics2D, Graphics, Color, EventQueue}
import javax.swing.JComponent
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.Txn
import prefuse.{Display, Visualization}
import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.action.{RepaintAction, ActionList}
import prefuse.activity.Activity
import prefuse.controls.{DragControl, ZoomToFitControl, PanControl, WheelZoomControl, ZoomControl}
import javax.swing.event.{AncestorEvent, AncestorListener}
import prefuse.data
import prefuse.visual.VisualItem
import de.sciss.lucre.expr.SpanLike
import prefuse.action.assignment.ColorAction
import prefuse.util.ColorLib
import prefuse.render.{LabelRenderer, DefaultRendererFactory}
import prefuse.util.force.{AbstractForce, ForceItem, CircularWallForce}

object VisualInstantPresentationImpl {
   def apply[ S <: Sys[ S ], A ]( transport: S#Entry[ A ])
                                ( implicit cursor: Cursor[ S ],
                                  transportView: A => Transport[ S, Proc[ S ]]) : VisualInstantPresentation[ S ] = {

      require( EventQueue.isDispatchThread, "Must be called on EDT" )

      val vis = new Impl( transport, cursor, transportView )
      cursor.step { implicit tx =>
         val map     = tx.newInMemoryIDMap[ Map[ SpanLike, List[ VisualProc ]]]
         val t       = transportView( transport.get )
         val all     = t.iterator.toIndexedSeq

         addRemove( all, IIdxSeq.empty )
//         t.changed.react {
//            case BiGroup.Added(   group, span, elem ) =>
//            case BiGroup.Removed( group, span, elem ) =>
//            case BiGroup.Moved(   group, changes ) =>
//            case BiGroup.Element( group, changes ) =>
//         }

         def onEDT( thunk: => Unit )( implicit tx: S#Tx ) {
            Txn.afterCommit( _ => EventQueue.invokeLater( new Runnable {
               def run() {
                  thunk
               }
            }))( tx.peer )
         }
         def playStop( b: Boolean )( implicit tx: S#Tx ) {
            onEDT( vis.playing = b )
         }

         def addRemove( added:   IIdxSeq[ (SpanLike, Proc[ S ])],
                        removed: IIdxSeq[ (SpanLike, Proc[ S ])])( implicit tx: S#Tx ) {
            val vpRem = removed.flatMap { case (span, proc) =>
               map.get( proc.id ).flatMap { vpm =>
                  map.remove( proc.id )
                  vpm.get( span ).flatMap {
                     case vp :: tail =>
                        if( tail.nonEmpty ) {
                           map.put( proc.id, vpm + (span -> tail) )
                        }
                        Some( vp )
                     case _ =>
                        None
                  }
               }
            }
            val hasRem = vpRem.nonEmpty
            val vpAdd = added.map { case (span, proc) =>
               val n    = proc.name.value
               val vp   = new VisualProc( n )
               map.get( proc.id ) match {
                  case Some( vpm ) =>
                     map.remove( proc.id )
                     map.put( proc.id, vpm + (span -> (vp :: vpm.getOrElse( span, Nil ))))
                  case _ =>
                     map.put( proc.id, Map( span -> (vp :: Nil) ))
               }
               vp
            }
            val hasAdd = vpAdd.nonEmpty
            if( hasAdd || hasRem ) onEDT {
               if( hasAdd ) vis.add(    vpAdd: _* )
               if( hasRem ) vis.remove( vpRem: _* )
            }
         }

         t.changed.reactTx { implicit tx => {
            case Transport.Seek(    _, time, added, removed ) => addRemove( added, removed )
            case Transport.Advance( _, time, added, removed ) => addRemove( added, removed )
            case Transport.Play( _ ) => playStop( b = true  )
            case Transport.Stop( _ ) => playStop( b = false )
         }}
      }

      vis.init()
      vis
   }

   private final class VisualProc( val name: String )

   private val ACTION_COLOR   = "color"
   private val ACTION_LAYOUT  = "layout"
   private val GROUP_GRAPH    = "graph"
   private val GROUP_NODES    = "graph.nodes"
   private val GROUP_EDGES    = "graph.edges"
   private val LAYOUT_TIME    = 50
   private val colrPlay       = new Color( 0, 0x80, 0 )
   private val colrStop       = Color.black

   private final class Impl[ S <: Sys[ S ], A ]( transport: S#Entry[ A ], cursor: Cursor[ S ],
                                                 transportView: A => Transport[ S, Proc[ S ]])
   extends VisualInstantPresentation[ S ] {
      private var playingVar = false
//      private var vps      = Set.empty[ VisualProc ]
      private var nodeMap  = Map.empty[ VisualProc, data.Node ]

      private val g        = {
         val res = new data.Graph
         res.addColumn( VisualItem.LABEL, classOf[ String ])
         res
      }
      private val pVis     = {
         val res = new Visualization()
         res.addGraph( GROUP_GRAPH, g )
         res
      }
      private val display  = new Display( pVis ) {
         override protected def setRenderingHints( g: Graphics2D ) {
            g.setRenderingHint( RenderingHints.KEY_ANTIALIASING,      RenderingHints.VALUE_ANTIALIAS_ON )
            g.setRenderingHint( RenderingHints.KEY_RENDERING,         RenderingHints.VALUE_RENDER_QUALITY )
            // XXX somehow this has now effect:
            g.setRenderingHint( RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON )
            g.setRenderingHint( RenderingHints.KEY_STROKE_CONTROL,    RenderingHints.VALUE_STROKE_PURE )
         }
      }

      def view : JComponent = display

      def init() {
         val lay = new ForceDirectedLayout( GROUP_GRAPH )
         val fs = lay.getForceSimulator

         // a somewhat weird force that keeps unconnected vertices
         // within some bounds :)
         fs.addForce( new AbstractForce {
            private val x     = 0f
            private val y     = 0f
            private val r     = 150f
            private val grav  = 0.4f

            protected def getParameterNames : Array[ String ] = Array[ String ]()

            override def isItemForce = true

            override def getForce( item: ForceItem ) {
               val n = item.location
               val dx = x-n(0)
               val dy = y-n(1)
               val d = math.sqrt(dx*dx+dy*dy).toFloat
               val dr = r-d
               val v = grav*item.mass / (dr*dr)
               if( d == 0.0 ) return
               item.force(0) += v*dx/d
               item.force(1) += v*dy/d

//               println( "" + (dx/d) + "," + (dy/d) + "," + dr + "," + v )
            }
         })

         val lbRend = new LabelRenderer( VisualItem.LABEL )
         val rf = new DefaultRendererFactory( lbRend )
         pVis.setRendererFactory( rf )

         // colors
         val actionNodeStroke = new ColorAction( GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb( 255, 255, 255 ))
         val actionNodeFill   = new ColorAction( GROUP_NODES, VisualItem.FILLCOLOR, ColorLib.rgb( 0, 0, 0 ))
         val actionTextColor  = new ColorAction( GROUP_NODES, VisualItem.TEXTCOLOR, ColorLib.rgb( 255, 255, 255 ))

         // quick repaint
         val actionColor = new ActionList()
         actionColor.add( actionTextColor )
         actionColor.add( actionNodeStroke )
         actionColor.add( actionNodeFill )
         pVis.putAction( ACTION_COLOR, actionColor )

         val actionLayout = new ActionList( Activity.INFINITY, LAYOUT_TIME )
         actionLayout.add( lay )
         actionLayout.add( new RepaintAction() )
         pVis.putAction( ACTION_LAYOUT, actionLayout )
         pVis.alwaysRunAfter( ACTION_COLOR, ACTION_LAYOUT )

         // ------------------------------------------------

         // initialize the display
         display.setSize( 600, 600 )
         val origin = new java.awt.Point( 0, 0 )
         display.zoom( origin, 2.0 )
         display.panTo( origin )
         display.addControlListener( new ZoomControl() )
         display.addControlListener( new WheelZoomControl() )
         display.addControlListener( new ZoomToFitControl() )
         display.addControlListener( new PanControl() )
         display.addControlListener( new DragControl() )
         display.setHighQuality( true )

         display.setForeground( Color.WHITE )
         display.setBackground( Color.BLACK )

         display.addAncestorListener( new AncestorListener {
            def ancestorAdded( e: AncestorEvent ) {
               startAnimation()
            }

            def ancestorRemoved( e: AncestorEvent) {
               stopAnimation()
            }

            def ancestorMoved( e: AncestorEvent) {}
         })
      }

      def add( vps: VisualProc* ) {
//         vps ++= procs
         visDo {
            vps.foreach( add1 )
         }
      }

      def remove( vps: VisualProc* ) {
//         vps --= procs
         visDo {
            vps.foreach( rem1 )
         }
      }

      private def add1( vp: VisualProc ) {
         val pNode   = g.addNode()
//         val vi      = pVis.getVisualItem( GROUP_GRAPH, pNode )
//         vi.setString( VisualItem.LABEL, vp.name )
         pNode.setString( VisualItem.LABEL, vp.name )
         nodeMap    += vp -> pNode
      }

      private def rem1( vp: VisualProc ) {
         nodeMap.get( vp ) match {
            case Some( n ) =>
               g.removeNode( n )
               nodeMap -= vp
            case _ =>
         }
      }

      def playing : Boolean = playingVar
      def playing_=( b: Boolean ) {
         if( playingVar != b ) {
            playingVar = b
            display.setBackground( if( b ) colrPlay else colrStop )
            view.repaint()
         }
      }

      private def stopAnimation() {
         pVis.cancel( ACTION_COLOR )
         pVis.cancel( ACTION_LAYOUT )
      }

      private def startAnimation() {
         pVis.run( ACTION_COLOR )
      }

      private def visDo( thunk: => Unit ) {
         pVis.synchronized {
            stopAnimation()
            try {
               thunk
            } finally {
               startAnimation()
            }
         }
      }
   }
}
