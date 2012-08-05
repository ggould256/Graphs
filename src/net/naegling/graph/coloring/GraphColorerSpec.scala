package net.naegling.graph

import net.naegling.graph._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class GraphColorerSpec extends FlatSpec with ShouldMatchers {
  val sz = 10
  val colorers = List(BruteForceColorer, ProgressiveColorer)
  val graphs = // a reproducible random set
    (( 0 until 10) map {x:Int => RandomGraph.undirected(sz, 2, false, new Random(x))}) ++
    ((10 until 20) map {x:Int => RandomGraph.undirected(sz, 2, true,  new Random(x))}) ++
    ((20 until 30) map {x:Int => RandomGraph.undirected(sz, 4, false, new Random(x))}) ++
    ((30 until 40) map {x:Int => RandomGraph.undirected(sz, 4, true,  new Random(x))})

  "a graph coloring" should "color every node" in {
    for (c <- colorers; g <- graphs) {
      val coloring = c.color(g)
      coloring should not be (None)
      (c.color(g).orNull.keySet should be (g.nodes))
    }
  }

  "a graph coloring" should "not color adjacent nodes the same" in {
    for (c <- colorers; g <- graphs) {
      val coloring = c.color(g)
      coloring should not be (None)
      for (e <- g.edges)
        coloring.orNull.apply(e._1) should not be (coloring.orNull.apply(e._2))
    }
  }

  "a graph coloring bounded in number of colors" should "use no more colors than allowed" in {
    for (c <- colorers; g <- graphs) {
      val coloring = c.color(g,3)
      if (coloring != None)
        for (n <- g.nodes)
          coloring.orNull.apply(n) should be < (3)
    }
  }

  // nodeOrder has no specified effect on the result of color() -- it is
  // purely a performance knob and so is not tested here except for a trivial
  // error condition

  "a coloring with a node order" should "require that each node appear exactly once" in {
    for (c <- colorers; g <- graphs) {
      evaluating { c.color(g, 3, (1 to (sz-1))) } should produce [Exception]
      evaluating { c.color(g, 3, (1 to (sz+1))) } should produce [Exception]
    }
  }
}
