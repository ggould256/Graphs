package net.naegling.apps

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
    response.setStatusCode(200)
    response.setContentString("Hello World")
    Future(response)
  }
}