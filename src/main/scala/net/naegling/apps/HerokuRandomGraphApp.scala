package net.naegling.apps

import net.naegling.graph._
import net.naegling.graph.coloring._

import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future
import java.net.InetSocketAddress
import util.Properties

object HerokuRandomGraphApp {
  def main(args: Array[String]) {
    val port = Properties.envOrElse("PORT", "8080").toInt
    println("Starting on port: "+port)
    ServerBuilder()
      .codec(Http())
      .name("hello-server")
      .bindTo(new InetSocketAddress(port))
      .build(new HerokuRandomGraphApp)
    println("Started.")
  }
}

class HerokuRandomGraphApp extends Service[HttpRequest, HttpResponse] {
  def apply(req: HttpRequest): Future[HttpResponse] = {
    val response = Response()
    val graph = RandomGraph.undirected(20,3)
    val remappedGraph = new UndirectedGraph(
        graph.nodes map {_.toString},
        graph.edges map {e => (e._1.toString, e._2.toString)}
      )
    val coloring = ProgressiveColorer.color(remappedGraph)
    val coloredGraphString = coloring match {
      case Some(c) => remappedGraph.toDot(
          {GraphColorer.colorMetadataFunction(c, _)}
        )
      case None => "No coloring found"
    }

    response.setStatusCode(200)
    response.setContentString(coloredGraphString)
    Future(response)
  }
}