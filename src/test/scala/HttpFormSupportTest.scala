package com.github.fntzr.spray.routing.ext.test.formsupport

import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import com.github.fntzr.spray.routing.ext._


case class Model10(id: Int, title: Option[String] = None)

trait HttpFormController extends BaseController {
  import HttpService._

  def post(m: Model10) = {
    val id = m.id
    val title = m.title.get
    complete(s"${id}${title}")
  }

  def another(m: Map[String, Any]) = {
    complete(s"${m.get("title").get.asInstanceOf[Option[String]].get}")
  }
}



trait HttpFormSupportTestable extends Routable {
  val route  = postForm[HttpFormController, Model10]("another", "id") ~
               postForm[HttpFormController, Model10]("z" ~> "post")
}

class HttpFormSupportTest extends FunSpec with Matchers with ScalatestRouteTest with HttpFormSupportTestable {
  def actorRefFactory = system

  describe("form support") {
    it("should pass map to form") {
      Post("/another", FormData(Seq("title" -> "spray"))) ~> route ~> check {
        responseAs[String] should startWith("spray")
      }
    }
    it("should pass model to form") {
      Post("/z", FormData(Seq("id" -> "10", "title" -> "spray"))) ~> route ~> check {
        responseAs[String] should startWith("10spray")
      }
    }
  }
}


