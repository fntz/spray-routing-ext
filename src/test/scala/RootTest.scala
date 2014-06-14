package com.github.fntzr.spray.routing.ext
import org.scalatest._
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._

trait RootController {
  import HttpService._
  def index = {
    complete{"index"}
  }
}

trait RootTestable extends Routable {
  val route =
    root[RootController]("index") ~
    pathPrefix("css") {
      complete {
        "css"
      }
    }
}

class RootTest extends FunSpec with Matchers with ScalatestRouteTest with RootTestable {
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
