package com.github.fntzr.spray.routing.ext
import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._


trait DI {
  val a: Int
  val b: String
  val xs : List[String]
}

trait DIController extends BaseController with DI {
  import HttpService._

  def method = {
    val r = s"$a$b${xs.size}"
    complete{r}
  }
}


trait DITestable extends Routable {
  def route(a: Int, b: String, xs: List[String]): Route = {
    get0[DIController]("method")
  }
}

class DITest extends FunSpec with Matchers with ScalatestRouteTest with DITestable {
  def actorRefFactory = system

  val z = route(1, "qwerty", List[String]("1", "2", "3"))

  describe("di test") {
    it ("test") {
      Get("/method") ~> z ~> check {
        responseAs[String] should startWith("1qwerty3")
      }
    }
  }
}
