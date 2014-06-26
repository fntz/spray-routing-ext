package com.github.fntzr.spray.routing.ext.test.root
import org.scalatest._
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._
import com.github.fntzr.spray.routing.ext._

trait Controller {
  import HttpService._
  def index = {
    complete{"index"}
  }
}

trait Routing extends Routable {
  val route =
    root[Controller]("index") ~
    pathPrefix("css") {
      complete {
        "css"
      }
    }
}

class RootTest extends FunSpec with Matchers with ScalatestRouteTest with Routing {
  def actorRefFactory = system

  describe("root test") {
    it ("should be a root url") {
      Get("/") ~> route ~> check {
        responseAs[String] should startWith("index")
      }
      Get("/css") ~> route ~> check {
        responseAs[String] should startWith("css")
      }
    }
  }
}
