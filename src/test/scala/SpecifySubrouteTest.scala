package com.github.fntzr.spray.routing.ext.test.specifysubroute
import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import com.github.fntzr.spray.routing.ext._

case class Model0(title: String)
case class Model1(title: String)
case class Model2(title: String)



trait Controller extends BaseController {
  import HttpService._

  def show(s: String) = {
    complete{s"show${s}"}
  }

  def update(s: String) = {
    complete{s"update$s"}
  }

  def edit(s: String) = {
    complete{s"edit$s"}
  }

  def delete(s: String)= {
    complete{s"delete$s"}
  }

  def create(model: Model1) = {
    complete{s"create ${model.title}"}
  }

  def fresh = {
    complete{"fresh"}
  }

  def index = {
    complete{"index"}
  }

}



trait Controller2 extends BaseController {
  import HttpService._

  def show(d: Double) = {
    complete{s"show${d}"}
  }

  def update(d: Double) = {
    complete{s"update$d"}
  }

  def edit(d: Double) = {
    complete{s"edit$d"}
  }

  def delete(d: Double)= {
    complete{s"delete$d"}
  }

}

trait Routing extends Routable {
  val route = resource[Controller, Model0](exclude("new", "create", "index"), Segment) ~
              resource[Controller2, Model2](exclude("new", "create", "index"), {
                pathPrefix("z") {
                  get {
                    complete{"z"}
                  }
                }
              }, DoubleNumber) ~
              resource[Controller, Model1](Segment, {
                pathPrefix("z") {
                  get {
                    complete{"z"}
                  }
                }
              })
}

class SpecifySubrouteTest extends FunSpec with Matchers with ScalatestRouteTest with Routing {
  def actorRefFactory = system

  describe("route with subroute") {
    it("when exclude with subroute") {
      Get("/model0/some-title/edit") ~> route ~> check{
        responseAs[String] should startWith("editsome-title")
      }
      Get("/model0/some-title") ~> route ~> check{
        responseAs[String] should startWith("showsome-title")
      }
      Put("/model0/some-title") ~> route ~> check{
        responseAs[String] should startWith("updatesome-title")
      }
      Delete("/model0/some-title") ~> route ~> check{
        responseAs[String] should startWith("deletesome-title")
      }
    }

    it("when block with subroute") {
      Get("/model1/some-title/edit") ~> route ~> check{
        responseAs[String] should startWith("editsome-title")
      }
      Get("/model1/some-title") ~> route ~> check{
        responseAs[String] should startWith("showsome-title")
      }
      Put("/model1/some-title") ~> route ~> check{
        responseAs[String] should startWith("updatesome-title")
      }
      Delete("/model1/some-title") ~> route ~> check{
        responseAs[String] should startWith("deletesome-title")
      }
    }

    it("when exclude and block and subroute") {
      Get("/model2/1.3/edit") ~> route ~> check{
        responseAs[String] should startWith("edit1.3")
      }
      Get("/model2/1.5") ~> route ~> check{
        responseAs[String] should startWith("show1.5")
      }
      Put("/model2/3.19") ~> route ~> check{
        responseAs[String] should startWith("update3.19")
      }
      Delete("/model2/10.0") ~> route ~> check{
        responseAs[String] should startWith("delete10.0")
      }
    }
  }
}
