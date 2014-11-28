package com.github.fntzr.spray.routing.ext.test.respondto

import org.scalatest._
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.MediaTypes._
import spray.http.StatusCodes.BadRequest
import com.github.fntzr.spray.routing.ext._
import spray.http.StatusCodes._

trait Controller extends BaseController with RespondToSupport {
  import HttpService._
  def show = {
    respondTo {
      case `text/plain` => "plain text"
      case `application/xml`  => "sad"
    }
  }
}

trait Routing extends Routable {
  val route = get0[Controller]("show")
}

class RespondTest extends FunSpec with Matchers with ScalatestRouteTest with Routing {
  def actorRefFactory = system

  describe("respond to") {
    it("complete by accept header") {
      Get("/show") ~> addHeader("Accept", "text/plain") ~> route ~> check {
        responseAs[String] should startWith("plain text")
      }
      Get("/show") ~> addHeader("Accept", "application/xml") ~> route ~> check {
        responseAs[String] should startWith("sad")
      }
      Get("/show") ~> addHeader("Accept", "application/json") ~> sealRoute(route) ~> check {
        status should be(NotAcceptable)
      }
      Get("/show") ~> sealRoute(route) ~> check {
        status should be(NotAcceptable)
      }
    }
  }
}
