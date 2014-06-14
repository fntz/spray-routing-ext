package com.github.fntzr.spray.routing.ext
import org.scalatest._
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.MediaTypes._
import spray.http.StatusCodes.BadRequest

trait MyRespondController extends BaseController with RespondToSupport {
  import HttpService._
  def show = {
    respondTo {
      case `text/css`    => "css"
      case `text/plain`  => "text"
      case  _            => "?"
    }
  }
}

trait RespondRoutes extends Routable {
  val route = get0[MyRespondController]("show")
}

class RespondTest extends FunSpec with Matchers with ScalatestRouteTest with RespondRoutes {
  def actorRefFactory = system

  describe("respond to") {
    it("complete by accept header") {
      Get("/show") ~> addHeader("Accept", "text/plain") ~> route ~> check {
        responseAs[String] should startWith("text")
      }
      Get("/show") ~> addHeader("Accept", "text/css,*/*;q=0.1") ~> route ~> check {
        responseAs[String] should startWith("css")
      }
      Get("/show") ~> addHeader("Accept", "text/xml") ~> sealRoute(route) ~> check {
        status should be(BadRequest)
      }
      Get("/show") ~> sealRoute(route) ~> check {
        status should be(BadRequest)
      }
    }
  }
}
