
import org.scalatest._
import spray.testkit.ScalatestRouteTest

trait A {
  def index = "1"
}

trait B extends Routable {
  val b = custom[A]("z" ~> "index")
  //val b = custom[A](("z" / Segment) ~> "index")
}

object RoutableTest extends FunSpec with Matchers with ScalatestRouteTest with B {
  def actorRefFactory = system
  describe("match0") {
    it ("1") {
      Get("/z/1") ~> b ~> check {
        responseAs[String] should startWith("1")
      }
    }
  }
}
