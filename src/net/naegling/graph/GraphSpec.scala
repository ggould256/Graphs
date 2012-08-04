package net.naegling.graph

import net.naegling.graph._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphSpec extends FlatSpec with ShouldMatchers {
  type IntGraph = UndirectedGraph[Int]
  type StringGraph = UndirectedGraph[String]
  val rgSize = 40
  val rg = RandomGraph.undirected(rgSize, 0.3f)

  "An undirected graph" should "have a set of nodes" in {
    new IntGraph(Set(1),Set()).nodes should contain (1)
    new IntGraph(Set(1),Set()).nodes should not contain (2)
  }

  it should "have edges only between distinct pairs of member nodes" in {
    evaluating { new IntGraph(Set(1),Set((1,2))) } should produce [InvalidEdgeException]
    evaluating { new IntGraph(Set(1,2),Set((1,1))) } should produce [InvalidEdgeException]    
  }

  it should "be undirected" in {
    val testGraph = new IntGraph(Set(1,2),Set((1,2)))
    testGraph.edges should have size (2)
    for (e <- rg.edges) (rg.edges should contain (e._2, e._1))
  }
  
  "g.arbitraryNode" should "return a node in g" in {
    rg.nodes should contain (rg.arbitraryNode)
  }

  "g edgesOf n" should "return all and only edges in g adjacent to n" in {
	for (n <- rg.nodes) {
	  for (e <- rg.edgesInto(n)) rg.edges should contain (e)
	  for (e <- rg.edges if (e._2 == n)) rg.edgesInto(n) should contain (e)
	  for (e <- rg.edgesInto(n)) n should (equal (e._1) or equal (e._2))
	}
  }

  "g neighborsOf n" should "return all and only those nodes m for which m--n is in g" in {
	for (n <- rg.nodes) {
	  for (m <- rg.neighborsOf(n)) rg.nodes should contain (m)
	  for (m <- rg.neighborsOf(n)) rg.edges should (contain ((n,m)) or contain ((m,n)))
	  for (e <- rg.edges) if (e._1 == n) rg.neighborsOf(n) should contain (e._2)
	  for (e <- rg.edges) if (e._2 == n) rg.neighborsOf(n) should contain (e._1)
	}
  }

  "g.isEmpty" should "return true if g is empty, false otherwise" in {
    new IntGraph(Set(),Set()) should be ('empty)
    new IntGraph(Set(1),Set()) should not be ('empty)    
  }

  "g arityOf n" should "return the number of edges in g connected from n" in {
	for (n <- rg.nodes)
	  rg arityOf n should equal (
	    (for (e <- rg.edges if (e._1 == n)) yield e) size
	  )
  }

  "g addNode n" should "return a new Graph with n added unconnected" in {
    val withNegOne = rg add -1
    withNegOne.nodes should contain (-1)
    withNegOne arityOf -1 should be (0)
    withNegOne neighborsOf -1 should be ('empty)
    withNegOne edgesInto -1 should be ('empty)
    evaluating {rg add (rg.arbitraryNode)} should produce [InvalidNodeException]
  }

  "g addEdge e" should "return a new Graph with edge e added" in {
	val anchor = rg.arbitraryNode
    val withNegOne = rg add -1 add (anchor,-1)
    withNegOne arityOf -1 should be (1)
    withNegOne neighborsOf -1 should equal (Set(anchor))
    withNegOne edgesInto -1 should not be ('empty)
    evaluating {withNegOne add (anchor, -1)} should produce [InvalidEdgeException]
    evaluating {withNegOne add (anchor, -2)} should produce [InvalidEdgeException]
  }

  "g rename f" should "return a new Graph with nodes remapped by f" in {
    def f(x:Int) = x * 10 + 1
    def fInv(x:Int) = (x - 1) / 10
    val renamed = rg rename f
    renamed.nodes.size should equal (rg.nodes.size)
    renamed.edges.size should equal (rg.edges.size)
    for (n <- rg.nodes) renamed neighborsOf f(n) should equal (rg neighborsOf n map f)
    renamed rename fInv should equal (rg)
  }
  "g rename f" should "require that f not produce duplicates" in {
	evaluating {rg rename (n => -1)} should produce[InvalidNodeException]
  }

  "g unsafeUnion h (for g, h disjoint)" should "return a graph with g and h as subgraphs" in {
	val testGraph = new IntGraph(Set(-1,-2),Set((-1, -2)))    
	assert(rg.nodes intersect testGraph.nodes isEmpty) // or else the test is invalid
	(rg unsafeUnion testGraph).nodes should equal (rg.nodes union testGraph.nodes)
	(rg unsafeUnion testGraph).edges should equal (rg.edges union testGraph.edges)
  }

  "g safeUnion h" should "return a graph with renamed versions of g and h as subgraphs" in {
	val testGraph = new IntGraph(Set(-1,-2),Set((-1, -2)))
	(rg safeUnion(testGraph,{_+100},{_+500})).nodes.size should equal (rg.nodes.size + testGraph.nodes.size)
	(rg safeUnion(testGraph,{_+100},{_+500})).edges.size should equal (rg.edges.size + testGraph.edges.size)
	// XXX should be some test of isomorphism here
  }

  "g subgraphWith ns" should "return a graph with only ns and their edges within g" in {
    val testGraph = rg subgraphWith Set(0,1,2,3)
    testGraph.nodes.size should be (4)
    testGraph.edges.size should be <= (6)
    for (e <- rg.edges if (Set(e._1,e._2) subsetOf testGraph.nodes))
      testGraph.edges should contain (e)
  }

  "g subgraphWith ns" should "return a graph without ns or their neighboring edges within g" in {
	val boundary = 5
    val testGraph = rg subgraphWithout (boundary until rgSize toSet)
    testGraph should equal (rg subgraphWith (0 until boundary toSet))
  }
}
