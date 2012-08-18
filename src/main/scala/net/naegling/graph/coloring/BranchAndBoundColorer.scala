package net.naegling.graph.coloring

import scala.collection.immutable.SortedSet
import scala.math.Ordering

import net.naegling.graph._

/**
 * Branch-and-bound is a technique for traversing trees if the nodes of a
 * tree can each be bounded above and below in the "badness" of the leaves of
 * that subtree.  Subtrees can be discarded when their roots' lower bound
 * is worse than the upper bound of any other node.
 * 
 * This is about two orders of magnitude better than than the progressive
 * colorer at the boundaries of a 2010-era laptop.
 */
object BranchAndBoundColorer extends GraphColorer {
  
  def _colorInternal[Node](
    graph : UndirectedGraph[Node],
    maxColors : Int,
    nodeOrder : Seq[Node]
  ) : Option[Map[Node, Int]] = {
    type Hypo = Hypothesis[Node]

    /**
     * The order in which to consider hypotheses.  In general branch-and-bound
     * systems prefer branches with smaller distances between their upper and
     * lower bounds as these are likely to terminate quickly.  In this case
     * however ties on this metric will be common so we will break ties toward
     * the better upper bound in hopes of lowering the upper bound.
     */
    object hypoOrder extends Ordering[Hypo] {
      def compare(a : Hypo, b : Hypo ) = {
        val aWidth = a.colorsUpperBound - a.colorsLowerBound
        val bWidth = b.colorsUpperBound - b.colorsLowerBound
        val comp = aWidth compare bWidth
        if (comp != 0)
          comp
        else
          (a.colorsUpperBound compare b.colorsUpperBound)
      }
    }
 
    var globalUpperBound = graph.nodes.size
    val baseHypo = new Hypo(Map(), nodeOrder, graph, 0)
    var toBeProcessed = SortedSet.empty(ord=hypoOrder)

    toBeProcessed = toBeProcessed + baseHypo
    while(true) {
      // check the out-of-ideas failure condition
      if (toBeProcessed.isEmpty) return None
      
      // filter the hypotheses to the global upper bound
      toBeProcessed =
        toBeProcessed filter { _.colorsLowerBound <= globalUpperBound }
      
      // pop a hypothesis off the top of the sorted set
      val hypo = toBeProcessed.head
      val remainingHypos = toBeProcessed.tail
      
      // expand that hypothesis
      val nextHypos = extendHypothesis(hypo, maxColors)
      nextHypos foreach { h => if (h.complete) return Some(h.colors) }
      toBeProcessed = remainingHypos | nextHypos
      
      // adjust the global upper bound
      globalUpperBound = 
        (List(globalUpperBound) ++ (toBeProcessed map { _.colorsUpperBound })) min
    }
    sys.error("Unreachable code reached") // unreachable due to while(true)
  }
}
