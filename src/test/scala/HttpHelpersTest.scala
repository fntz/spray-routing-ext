package spray.routing.ext
import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._

trait HttpHelpersController extends BaseController {
  import HttpService._

  def foo = {
    complete{"foo"}
  }

  def baz = {
    complete{"baz"}
  }

  def root = {
    complete{"root"}
  }
}

trait HttpHelpersTestable extends Routable {
  import spray.http.HttpMethods._
  val route = scope("scope") {
    get0[HttpHelpersController]("foo") ~
    scope("nested") {
      get {
        complete("nested")
      }
    } ~
    match0[HttpHelpersController]("baz0" ~> "baz", List(GET, PUT)) ~
    match0[HttpHelpersController]("baz") ~
    match0[HttpHelpersController]("baz1" ~> "baz")
  }

  val rt = root[HttpHelpersController]("root")

}

class HttpHelpersTest extends FunSpec with Matchers with ScalatestRouteTest with HttpHelpersTestable {
  def actorRefFactory = system

  describe("scope") {
    it("should call controller methods which nested into scope") {
      Get("/scope/foo") ~> route ~> check {
        responseAs[String] should startWith("foo")
      }
    }
    it("should have a nested scope") {
      Get("/scope/nested") ~> route ~> check {
        responseAs[String] should startWith("nested")
      }
    }
  }

  describe("match0") {

    it("should handle request with passed actions") {
      Get("/scope/baz0") ~> route ~> check {
        responseAs[String] should startWith("baz")
      }
      Put("/scope/baz0") ~> route ~> check {
        responseAs[String] should startWith("baz")
      }
      Delete("/scope/baz0") ~> route ~> check {
        handled should be(false)
      }
    }

    it("should handle with GET by default and only action name as path") {
      Get("/scope/baz") ~> route ~> check {
        responseAs[String] should startWith("baz")
      }
      Delete("/scope/baz") ~> route ~> check {
        handled should be(false)
      }
      Put("/scope/baz0") ~> route ~> check {
        responseAs[String] should startWith("baz")
      }
    }

    it("should handle when mathc0 without httpmethods") {
      Put("/scope/baz1") ~> route ~> check {
        handled should be(false)
      }
      Post("/scope/baz1") ~> route ~> check {
        handled should be(false)
      }
      Delete("/scope/baz1") ~> route ~> check {
        handled should be(false)
      }
      Get("/scope/baz1") ~> route ~> check {
        responseAs[String] should startWith("baz")
      }
    }
  }

  describe("root") {
    it("handle root with get method") {
      Get("/") ~> rt ~> check {
        responseAs[String] should startWith("root")
      }
    }
  }
}
