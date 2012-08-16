package net.naegling.graph

/**
 * A superclass for graph coloring algorithms.  Subclasses must implement
 * color(); its arguments are:
 *
 *   graph -- the undirected graph to be colored
 *   maxColors -- the maximum number of colors to be tried; set this
 *                to bound computational complexity.  By default there
 *                is no bound (maxColors == graph.nodes.size)
 *   nodeOrder -- the order in which to consider the nodes; by default
 *                the order is arbitrary
 * returns:
 *   - a coloring (a map of Node -> Int for all nodes in graph) if one is
 *     possible within the maxColors constraint
 *   - None if no such coloring exists
 *   
 * The order parameter bears some additional explanation:  It is well 
 * documented as a pragmatic if not entirely well-theorized result that the
 * complexity of coloring and similar constraint solving problems varies
 * dramatically with the order in which nodes are considered (see eg
 * Zabih, 1990).  If a caller is in possession of a low-bandwidth ordering for
 * the graph, the exponent of the computational complexity may fall from the
 * number of colors to the ordering bandwidth, possibly saving many orders of
 * magnitude of runtime cost for large graphs of moderate density.
 */
abstract class GraphColorer {

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
    require (nodeOrder.toSet == graph.nodes)
    _colorInternal(graph, maxColors, nodeOrder)
  }

  /** The core function bit that a colorer should override */
  def _colorInternal[Node](
    graph : UndirectedGraph[Node],
    maxColors : Int,  //< defaults to g.nodes.size
    nodeOrder : Seq[Node] //< defaults to g.nodes.toSeq
  ) : Option[Map[Node, Int]]
  
  /** An internal class that represents a possible partial coloring of the 
   * given graph; provided as a convenience to colorer implementors. */
  class Hypothesis[Node] (
    val colors : Map[Node, Int],
    val remainingNodes : Seq[Node],
    val graph : UndirectedGraph[Node],
    val numColors : Int
  ) {
    def complete = remainingNodes.isEmpty
    def colorNext(color : Int) = new Hypothesis(
      colors + ((remainingNodes.head, color)),
      remainingNodes.tail,
      graph,
      scala.math.max(numColors, color + 1)
    )

    /** For branch-and-bound algorithms, the lowest possible number of colors
     * a coloring could color this graph with */
    def colorsLowerBound = numColors // with cliques we could improve this

    /** For branch-and-bound algorithms, a number of colors this graph is
     * guaranteed to be colorable with */
    def colorsUpperBound = numColors + remainingNodes.size
  }
  
  /**
   * Given an hypothesis, obtain all the consistent hypotheses for the
   * coloring of the next node in the sequence; provided as a convenience 
   * to colorer implementors.
   */
  def extendHypothesis[Node](
    h : Hypothesis[Node],
    maxColors : Int
  ) : Set[Hypothesis[Node]] = {
    if (h.remainingNodes.isEmpty) return Set()
    val nextNode = h.remainingNodes.head
    val adjacentColors =
      (h.graph neighborsOf h.remainingNodes.head) flatMap {h.colors.get(_)}
    val newColorHypo : Set[Hypothesis[Node]] =
      if (h.numColors < maxColors)
        Set(h colorNext h.numColors)
      else Set()
    val existingColorHypos =
      for (c <- 0 until h.numColors; if !(adjacentColors contains c))
        yield (h colorNext c)
    (existingColorHypos toSet) | newColorHypo
  }
}
