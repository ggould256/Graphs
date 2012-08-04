package net.naegling.graph

/**
 * A small improvement on the brute force colorer: Try coloring with smaller
 * numbers of colors first.  Worse worse-case complexity (because it does
 * strictly more work) but much, much better average case (because low-color
 * solutions are much faster than higher color ones)
 */
class ProgressiveGraphColorer extends GraphColorer {
  
  def color[Node](
    graph : UndirectedGraph[Node],
    _maxColors : Int = -1,  //< defaults to g.nodes.size
    _nodeOrder : Seq[Node] = null //< defaults to g.nodes.toSeq
  ) : Option[Map[Node, Int]] = {
    val maxColors = if (_maxColors == -1) graph.nodes.size else _maxColors
    val nodeOrder = if (_nodeOrder == null) graph.nodes.toSeq else _nodeOrder
    for (n <- 1 to maxColors) {
      val result = BruteForceColorer.color(graph, n, nodeOrder)
      if (result != None) return result
    }
    return None
  }
}
