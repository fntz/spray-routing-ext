package com.github.fntzr.spray.routing.ext.test.di
import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import com.github.fntzr.spray.routing.ext._


trait DI {
  val a: Int
  val b: String
  val xs : List[String]
}

trait Controller extends BaseController with DI {
  import HttpService._

  def method = {
    val r = s"$a$b${xs.size}"
    complete{r}
  }
}


trait Routing extends Routable {
  def route(a: Int, b: String, xs: List[String]): Route = {
    get0[Controller]("method")
  }
}

class DITest extends FunSpec with Matchers with ScalatestRouteTest with Routing {
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
