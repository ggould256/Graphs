package net.naegling.graph

import java.io.ByteArrayInputStream
import scala.collection.immutable.Set
import scala.collection.mutable.{Set => MSet}
import scala.sys.process._
import scala.util.Random
import scala.collection.mutable.Queue

/** This exception indicates failures due to an edge that cannot support
 * the given action in the given graph, eg cannot be created because it
 * exists or cannot be deleted because it does not. */
class InvalidEdgeException (
  badEdge : (Any,Any),
  message : String = "invalid edge"
) extends Exception {
  override def toString = "InvalidEdgeException(" + badEdge + ", " + message + ")"
}

/** This exception indicates failures due to an node that cannot support
 * the given action in the given graph, eg cannot be created because it
 * exists or cannot be deleted because it does not. */
class InvalidNodeException (
  badNode : Any,
  message : String = "invalid node"
) extends Exception {
  override def toString = "InvalidNodeException(" + badNode + ", " + message + ")"
}

/**
 * A trait defining the methods in common between directed and undirected
 * graphs.  A class extending this trait will need at minimum to define a
 * _factory method to allow the functional updaters to recreate the target
 * class and a canEqual method to avoid equality leaking across subclass
 * boundaries.  Overriding hashCode is helpful but not required.
 * 
 * The default behaviour is a straightforward undirected graph via adjacency
 * set; subclasses requiring different behaviour or implementation will need
 * to override one or more other methods.
 * 
 * The somewhat cryptic type signature (for a given GraphLike type T, defines 
 * the type parameter "This" bounded above by by T) is to ensure that the
 * functional updaters return the type of the original object -- ie that
 * UndirectedGraph.add() returns UndirectedGraph rather than GraphLike or
 * DirectedGraph or something similar.
 */
trait GraphLike[Node, This <: GraphLike[Node,This]] {
  type Edge = (Node, Node)
  
  def nodes : Set[Node]
  def edges : Set[Edge]

  /** subclasses need to define a way for this trait to create a new "This" */
  def _factory(ns : Set[Node], es : Set[Edge]) : This
  
  /** true iff n is a node in this graph */
  def contains(n : Node) : Boolean = nodes(n)

  /** true iff e is an edge in this graph */
  def contains(e : Edge) : Boolean = edges(e)

  /** returns a copy of this graph also containing node n; if n is already
   *  present then this graph is returned unchanged */
  def maybeAdd(n : Node) : This = _factory(nodes + n, edges)

  /** returns a copy of this graph also containing edge e; if e is already
   *  present then this graph is returned unchanged. It is an error if the
   *  ends of the given edge are not in the graph
   */  
  def maybeAdd(e : Edge) : This = {
    if (!nodes(e._2)) throw new InvalidEdgeException(e)
    if (!nodes(e._1)) throw new InvalidEdgeException(e)
    _factory(nodes, edges + e)
  }
  
  /** returns a copy of this graph also containing node n; it is an error if
   *  if n is already present */
  def add(n : Node) : This = {
    if (nodes(n)) throw new InvalidNodeException(n)
    maybeAdd(n)
  }
  
  /** returns a copy of this graph also containing edge e; it is an error
   *  if e is already present then this graph or if the ends of the given 
   *  edge are not in the graph
   */
  def add(e : Edge) : This = {
    if (edges(e)) throw new InvalidEdgeException(e)
    maybeAdd(e)
  }

  /** returns a copy of this graph lacking node n; if n is already absent 
   * then this graph is returned unchanged */
  def remove(n : Node) : This = _factory(nodes - n, edges)

  /** returns a copy of this graph lacking edge e; if e is already absent 
   * then this graph is returned unchanged */
  def remove(e : Edge) : This = _factory(nodes, edges - e)

  /** returns a node in this graph; it is an error if this graph is empty */
  def arbitraryNode = nodes.head

  /** returns all edges originating at node n; if n is not present then 
   * returns the empty set */
  def edgesFrom(n : Node) : Set[Edge] =
    for (e <- edges if (e._1 == n)) yield e

  /** returns all edges into node n; if n is not present then 
   * returns the empty set */
  def edgesInto(n : Node) : Set[Edge] =
    for (e <- edges if (e._2 == n)) yield e

  /** returns all nodes with an edge into n */
  def predecessorsOf(n : Node) : Set[Node] = edgesInto(n) map {_._1}

  /** returns all nodes into which n has an edge */
  def successorsOf(n : Node) : Set[Node] = edgesFrom(n) map {_._2}

  /** returns all nodes with edges to or from n */
  def neighborsOf(n : Node) : Set[Node] =
    predecessorsOf(n) union successorsOf(n)

  /** true iff this graph has no nodes (and therefore no edges) */
  def isEmpty = nodes.isEmpty

  /** returns the number of edges out of node n */
  def arityOf(n : Node) = successorsOf(n) size

  /** returns the maximum arity of any node in the graph */
  def maximumArity = (nodes map arityOf) max

  /** returns an arbitrary node having the maximum arity */
  def maximumArityNode = nodes maxBy arityOf

  /** returns all nodes in the graph having the graph's maximum arity */
  def maximumArityNodes = nodes filter { arityOf(_) == maximumArity }

  /** returns a copy of this graph with the node labels transformed by
   * the given function. */
  def rename(f : (Node => Node)) : This = {
    val dups = (nodes.toList map f) diff (nodes.toList map f).distinct
    if (! dups.isEmpty) throw new InvalidNodeException(
        dups.head,
        "rename caused duplicate node "+dups.head
    )
    else _factory(
      nodes map f,
      edges map (e => (f(e._1), f(e._2)))
    )
  }
  
  /** Unions two graphs without regard to label overlap
   *  NOTE: This version of union collapses equal nodes between its
   *  arguments! */
  def unsafeUnion(g : This) = _factory(nodes | g.nodes, edges | g.edges)

  /** Unions two graphs while transforming the labels to avoid collisions. */
  def safeUnion(g : This, lXform : (Node => Node), rXform : (Node => Node)) = {
    val left = this rename lXform
    val right = g rename rXform
    if (! (left.nodes intersect right.nodes).isEmpty)
      throw new IllegalArgumentException("safeUnion prefixes must differ")
    else left unsafeUnion right
  }

  /** returns a subgraph of this graph consisting only of the indicated nodes
   * and the edges between those nodes */
  def subgraphWith(ns : Set[Node]) : This = {
    assert (ns subsetOf nodes)
    return _factory(ns, for (e <- edges if (ns contains e._1) && (ns contains e._2)) yield e)
  }

  /** returns a subgraph of this graph that is missing the indicated nodes and
   * any edges to, from, or between them.*/
  def subgraphWithout(ns : Set[Node]) : This = {
    assert (ns subsetOf nodes)
    return subgraphWith(nodes diff ns)
  }

  
  /**
   * Returns a string suitable for interpretation by the graph visualization
   * tool "dot".  If a metadata function is provided, passes through the
   * result for each node as dot metadata for that node (useful for passing in
   * a coloring, for example).
   */
  def toDot(metadataFn : (Node => String) = {_ => ""}) =
    "digraph {\n" +
    (for (n <- nodes) yield ("\t" + n + " " + metadataFn(n) + ";\n")).mkString +
    (for (e <- edges) yield ("\t" + (e._1) + " -> " + (e._2) + ";\n")).mkString +
    "}\n"

  /**
   * Displays a visualization of the graph.  If a metadata function is
   * provided, passes through the result for each node as dot metadata for that
   * node (useful for passing in a coloring, for example).
   * XXX Unfortunately this is MacOS-specific and requires graphviz ("dot")
   */
  def show(metadataFn : (Node => String) = {_ => ""}) {
    ("/usr/local/bin/dot -Tpdf" #<
      (new ByteArrayInputStream( toDot(metadataFn).getBytes("UTF-8")))
    ) #| "open -f -a /Applications/Preview.app" !
  }

  // *** Use data equality (not pointer equality) for immutable class ***
  override def equals(other: Any): Boolean = other match {
    case that: GraphLike[_,_] =>
      (that canEqual this) &&
      (that.hashCode == this.hashCode) &&
      nodes == that.nodes &&
      edges == that.edges
    case _ => false
  }

  /** subclasses MUST implement this to avoid equality across classes */
  def canEqual(other: Any): Boolean

  /** subclasses SHOULD implement this to avoid collisions across classes */
  override def hashCode: Int
}

/**
 * This class implements an immutable directed graph with nodes identified by
 * instances of the Node type parameter (eg, DirectedGraph[String] uses nodes
 * identified by string).
 * 
 * The implementation is via an adjacency set; it is therefore optimized for 
 * sparse graphs, cheap updates, and compatibility with published algorithms.
 *
 * Use of a mutable type for Node will cause unpredictable results and should
 * be avoided.
 */
class DirectedGraph[Node](
  val nodes : Set[Node] = Set[Node](),
  val edges : Set[(Node, Node)] = Set[(Node, Node)]()
) extends GraphLike[Node, DirectedGraph[Node]] {

  edges foreach {e => 
    if (!nodes(e._1)) throw new InvalidEdgeException(e)
    if (!nodes(e._2)) throw new InvalidEdgeException(e)      
    if (e._1 == e._2) throw new InvalidEdgeException(e)
  }

  def _factory(ns : Set[Node], es : Set[Edge]) = {
    new DirectedGraph[Node](ns,es)
  }

  override def toString =
    "DirectedGraph(" + nodes.toString + ", " + edges.toString + ")"

  def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedGraph[_]]
  override lazy val hashCode: Int = 41 * (41 + nodes.hashCode) + edges.hashCode
}

/**
 * This class implements an immutable undirected graph with nodes identified
 * by instances of the Node type parameter (eg, UndirectedGraph[String] uses
 * nodes identified by string).
 * 
 * The implementation is via an adjacency set; it is therefore optimized for 
 * sparse graphs, cheap updates, and compatibility with published algorithms.
 *
 * Use of a mutable type for Node will cause unpredictable results and should
 * be avoided.
 */
class UndirectedGraph[Node](
  val nodes : Set[Node] = Set[Node](),
  _edges : Set[(Node, Node)] = Set[(Node, Node)]()
) extends GraphLike[Node, UndirectedGraph[Node]] {
  val edges = _edges | (_edges map {_.swap})
  edges foreach {e => 
    if (!nodes(e._1)) throw new InvalidEdgeException(e)
    if (!nodes(e._2)) throw new InvalidEdgeException(e)      
    if (e._1 == e._2) throw new InvalidEdgeException(e)
  }

  def _factory(ns : Set[Node], es : Set[Edge]) = {
    new UndirectedGraph[Node](ns,es)
  }

  // remove both the edge and its reverse edge so that the constructor doesn't
  // recreate the reverse edge
  override def remove(e : Edge) =
    new UndirectedGraph[Node](nodes, edges - e - e.swap)

  override def toDot(metadataFn : (Node => String) = {_ => ""}) = {
    val undirectedEdges : Queue[(Node,Node)] = Queue()
    edges foreach {
      e => if (!(undirectedEdges contains e.swap)) undirectedEdges enqueue e
    }
    "graph {\n" +
    (for (n <- nodes) yield ("\t" + n + " " + metadataFn(n) + ";\n")).mkString +
    (for (e <- undirectedEdges)
      yield ("\t" + (e._1) + " -- " + (e._2) + ";\n")).mkString +
    "}\n"
  }
  
  override def toString =
    "UndirectedGraph(" + nodes.toString + ", " + edges.toString + ")"

  def canEqual(other: Any): Boolean = other.isInstanceOf[UndirectedGraph[_]]
  override lazy val hashCode: Int = 41 * (41 + nodes.hashCode) + edges.hashCode + 1
}

/** A source of random graphs for testing and demonstration */
object RandomGraph {
  /**
   * Generates a random directed graph.
   * Arguments:
   *  - size : number of nodes
   *  - averageArity : ratio of edges to nodes
   *  - connectedness : if true generates a connected graph (overrides
   *      averageArity to enforce connectedness)
   *  - prng : a random number generator, eg to force a specific seed
   */
  def directed(
    size : Int,
    averageArity : Float = 2,
    connected : Boolean = false, 
    prng : Random = new Random()
  ) : DirectedGraph[Int] = {
    type Edge = (Int,Int)
    assert (size > 0)
    val nodes = (0 until size) toSet
    var edges = MSet[(Int,Int)]()
    if (connected) {
      for (n <- nodes if n > 0) {
        val dest = prng nextInt n
        edges + ((n, dest))
      }
    }
    val allPossibleEdges =
      (for (n <- 0 until size; m <- 0 until n)
        yield ((m,n))
      ) toSet
    val edgeCount : Int = math.round((int2float(size)) * averageArity)
    val edgeList = prng shuffle ((allPossibleEdges diff edges) toSeq)
    edges |= (edgeList take (edgeCount - (edges size))).toSet
    return new DirectedGraph[Int](nodes, edges toSet)
  }

  /**
   * Generates a random undirected graph.
   * Arguments:
   *  - size : number of nodes
   *  - averageArity : ratio of edges to nodes
   *  - connectedness : if true generates a connected graph (overrides
   *      averageArity to enforce connectedness)
   *  - prng : a random number generator, eg to force a specific seed
   */
  def undirected(
    size : Int,
    averageArity : Float = 2,
    connected : Boolean = false, 
    prng : Random = new Random()
  ) : UndirectedGraph[Int] = {
    val r = directed(size, averageArity, connected, prng)
    new UndirectedGraph(r.nodes, r.edges)
  }
}

object TestGraphs {
  
  /** A 20-node undirected graph suitable for crashing a brute-force coloring */
  val bigGraph =
    new UndirectedGraph(
      Set(0, 5, 10, 14, 1, 6, 9, 13, 2, 17, 12, 7, 3, 18, 16, 11, 8, 19, 4, 15),
      Set((0,3), (4,2), (13,19), (17,2), (3,12), (11,16), (7,2), (2,11), (7,1), (16,3), (7,8), (4,10), (12,5), (12,3), (6,5), (8,18), (7,18), (13,12), (19,10), (11,9), (17,15), (5,12), (6,11), (2,17), (10,4), (2,4), (8,6), (8,10), (15,2), (14,10), (12,15), (9,6), (16,11), (1,15), (19,13), (7,12), (10,14), (13,2), (11,6), (15,12), (12,7), (3,14), (15,17), (3,8), (2,15), (15,1), (1,7), (16,2), (1,6), (3,16), (16,0), (10,12), (0,14), (11,2), (10,19), (2,16), (8,3), (6,9), (10,8), (11,12), (15,14), (14,3), (2,7), (12,13), (12,11), (3,0), (5,6), (18,12), (6,8), (14,0), (12,18), (8,7), (6,1), (9,11), (14,15), (18,7), (0,16), (2,13), (12,10), (18,8))
    )

  /** A 14-node subgraph of bigGraph that is just beyond the capabilities of
   * brute forcing on my development laptop. Also 3-colorable */
  val mediumGraph =
    bigGraph subgraphWith (6 until 20 toSet)

  /** A 10-node subgraph of bigGraph that 3-colors easily */
  val smallGraph =
    bigGraph subgraphWith (10 until 20 toSet)

}
