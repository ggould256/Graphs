package net.naegling.graph

import scala.collection.mutable.Queue

/**
 * A graph coloring algorithm that does a breadth-first exploration of
 * possible colorings until it finds a correct coloring.
 */
object BruteForceColorer extends GraphColorer {
  
  /**
   * Performs a coloring of the given graph, constrained by the given color
   * bound and operating on the given order if any
   */
  def color[Node](
    graph : UndirectedGraph[Node],
    _maxColors : Int = -1,  //< defaults to g.nodes.size
    _nodeOrder : Seq[Node] = null //< defaults to g.nodes.toSeq
  ) : Option[Map[Node, Int]] = {
    val maxColors = if (_maxColors == -1) graph.nodes.size else _maxColors
    val nodeOrder = if (_nodeOrder == null) graph.nodes.toSeq else _nodeOrder
    val baseHypo = new Hypothesis(Map(), nodeOrder, graph, 0)
    val processingQueue : Queue[Hypothesis[Node]] = Queue(baseHypo)
    var done = false
    while(!done) {
      if (processingQueue.isEmpty) return None
      val hypo = processingQueue.dequeue()
      val nextHypos = (extendHypothesis(hypo, maxColors) toSeq)
      nextHypos foreach { h =>
        if (h.complete) return Some(h.colors) else processingQueue.enqueue(h)
      }
    }
    sys.error("Unreachable code reached") // unreachable due to while(true)
  }
}