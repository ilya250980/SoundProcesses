package dotty.tools
package repl

import dotty.tools.dotc.core.Contexts.Context

class MyRendering(parentClassLoader: Option[ClassLoader] = None) extends Rendering(parentClassLoader) {
  override def classLoader()(using Context): ClassLoader = super.classLoader()
}