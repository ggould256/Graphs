package net.naegling.apps

import net.naegling.graph._

/**
 * A command-line application to color graphs; useful for manual testing of
 * graph and coloring implementations.  Takes a description of a graph from
 * stdin; emits that same graph marked up with colors on stdout.
 * 
 * options:
 *   -V ('visualize') to show the graph in a graphical form; works only on
 *      MacOS with graphviz installed.
 * 
 * The graph format is pseudo-graphviz -- "a -> b; c -- d;" etc.
 */
object GraphColorApp {

  type statementContent = Either[String, (String, Boolean, String)]

  def main(args: Array[String]): Unit = {
    val inputStream = System.in
    val input = // from stackoverflow: read stream to string + fix newlines
      scala.io.Source.fromInputStream(inputStream).getLines().mkString("\n")
    val statements = input.split(";")
    val contents = statements map parseStatement
    val dirGraph = contents.foldLeft(new DirectedGraph[String])(extend)
    val undirGraph = new UndirectedGraph(dirGraph.nodes, dirGraph.edges)
    val coloring = (new ProgressiveGraphColorer).color(undirGraph)
    
    System.out.println (
      coloring match {
        case Some(c) =>
          if (args contains "-V") undirGraph.show({colorMetadataFunction(c, _)})
          undirGraph.toDot({colorMetadataFunction(c, _)})
        case None =>
          "Coloring failed"
        }
    )
  }

  /** parse a declaration of a node (just a single word) or edge (two words
   * separated by an edge type, either '--' or '->' */
  def parseStatement(s : String) : statementContent = {
    val dirMatch = """\s*(\w+)\s*->\s*(\w+)""".r.findPrefixMatchOf(s)
    val undirMatch = """\s*(\w+)\s*--\s*(\w+)""".r.findPrefixMatchOf(s)
    val nodeMatch = """\s*(\w+)""".r.findPrefixMatchOf(s)
    if (dirMatch.isDefined)
      Right(dirMatch.orNull.group(1), true, dirMatch.orNull.group(2))
    if (undirMatch.isDefined)
      Right(undirMatch.orNull.group(1), true, undirMatch.orNull.group(2))
    else if (nodeMatch.isDefined)
      Left(nodeMatch.orNull.group(1))
    else sys.error("could not parse statement: "+s)
  }

  /** interpret a list of statements as a graph by unioning each one in turn
   * onto an empty graph */
  def extend(g : DirectedGraph[String], c : statementContent)
  	: DirectedGraph[String] = c match {
      case Left(s) => 
        g maybeAdd s
      case Right((l,true,r)) =>
        g maybeAdd l maybeAdd r maybeAdd (l,r) maybeAdd (r,l)
      case Right((l,false,r)) => 
        g maybeAdd l maybeAdd r maybeAdd (l,r)
    }

  /** produce graphviz color metadata */
  def colorMetadataFunction(coloring : Map[String,Int], node : String) : String = {
    val colorList = List("blue", "red", "green", "black", "magenta", "cyan", "yellow", "purple", "orange")
    "[ color=" + colorList(coloring(node)) + "]"
  }
}
