package com.github.fntzr.spray.routing.ext.test.httpmethods

import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._
import com.github.fntzr.spray.routing.ext._


trait Controller extends BaseController {
  import HttpService._

  def foo(id: Int) = {
    complete{id.toString}
  }

  def foo0(str: String) = {
    complete{str}
  }

  def foo1 = {
    complete{"pass"}
  }

  def baz = {
    complete{"baz"}
  }

  def custom = {
    respondWithHeader(RawHeader("MySuperHeader", "wow")) {
      complete{"custom"}
    }
  }

  def bar = {
    complete{"bar"}
  }
}



trait Routing extends Routable {
   val route  = get0[Controller](("foo" / IntNumber) ~> "foo")// ~
//    get0[Controller](("foo" / Segment) ~> "foo0") ~
//    get0[Controller]("foo" ~> "foo1") ~
//    get0[Controller]("baz") ~
//    get0[Controller]("custom")
}

class HttpMethodsTest extends FunSpec with Matchers with ScalatestRouteTest with Routing {
  def actorRefFactory = system

  describe("get0") {
    it("should take a parameter when parameter pass as part of path") {
      Get("/foo/42") ~> route ~> check {
        responseAs[String] should startWith("42")
      }
    }

//    it("should take a parameter when parameter as path") {
//      Get("/foo/bar") ~> route ~> check {
//        responseAs[String] should startWith("bar")
//      }
//    }
//
//    it("should call method from type") {
//      Get("/foo") ~> route ~> check {
//        responseAs[String] should startWith("pass")
//      }
//    }
//
//    it("should pass into default action for one argument") {
//      Get("/baz") ~> route ~> check {
//        responseAs[String] should startWith("baz")
//      }
//    }
//
//    it("should get headers from controller") {
//      Get("/custom") ~> route ~> check {
//        headers.filter(_.is("mysuperheader")) === List(RawHeader("MySuperHeader", "wow"))
//        responseAs[String] should startWith("custom")
//      }
//    }
  }
}

