package net.naegling.graph.coloring

import net.naegling.graph.UndirectedGraph

/**
 * A small improvement on the brute force colorer: Try coloring with smaller
 * numbers of colors first.  Worse worse-case complexity (because it does
 * strictly more work) but much, much better average case (because low-color
 * solutions are much faster than higher color ones)
 */
object ProgressiveColorer extends GraphColorer {
  
  def _colorInternal[Node](
    graph : UndirectedGraph[Node],
    maxColors : Int,
    nodeOrder : Seq[Node]
  ) : Option[Map[Node, Int]] = {
    for (n <- 1 to maxColors) {
      val result = BreadthFirstColorer.color(graph, n, nodeOrder)
      if (result != None) return result
    }
    return None
  }
}
