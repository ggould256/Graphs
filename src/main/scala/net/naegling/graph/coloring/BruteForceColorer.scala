package net.naegling.graph.coloring

import net.naegling.graph.UndirectedGraph
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
  def _colorInternal[Node](
    graph : UndirectedGraph[Node],
    maxColors : Int,
    nodeOrder : Seq[Node]
  ) : Option[Map[Node, Int]] = {
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