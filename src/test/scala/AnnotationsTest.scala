package com.github.fntzr.spray.routing.ext.test.annotations



import org.scalatest._
import spray.http.{FormData}
import spray.testkit.ScalatestRouteTest
import spray.routing._
import com.github.fntzr.spray.routing.ext.annotations._

trait MyController {

  import spray.routing.HttpService._

  @get("/foo")
  def get(z: String) = {
    complete("ok")
  }



}




class AnnotationsTest extends FunSpec with Matchers with ScalatestRouteTest with RouteBuild {
  def actorRefFactory = system

  describe("build") {
    it ("should build route") {
      buildRoute[MyController]

      "1" should startWith("1")
    }

  }

}
