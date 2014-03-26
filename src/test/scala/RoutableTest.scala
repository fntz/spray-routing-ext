package spray.routing.ext
import org.scalatest._
import spray.http.{RequestProcessingException, FormData}
import spray.routing.HttpService._
import spray.testkit.ScalatestRouteTest
import spray.routing._
import spray.http.HttpHeaders._


case class Model0(id: Int, title: String)
case class Model1(id: Int)
case class Model2(title: String)
case class Model3(id: Int)
case class Model4(id: Int, title: String)

trait Controller0 extends BaseController {
  import HttpService._

  def index = {
    complete{"index"}
  }

  def show(id: Int) = {
    complete{s"show${id}"}
  }

  def update(id: Int) = {
    complete{s"update$id"}
  }

  def edit(id: Int) = {
    complete{s"edit$id"}
  }

  def delete(id: Int)= {
    complete{s"delete$id"}
  }

  def create(model: Model0) = {
    complete{s"create ${model.id} - ${model.title}"}
  }

  def fresh = {
    complete{"fresh"}
  }
}

trait Controller1 extends BaseController {
  import HttpService._
  def fresh = {
    complete{"fresh"}
  }
}

trait Controller2 extends BaseController {
  def index = {
    complete{"index"}
  }

  def show(id: Int) = {
    complete{s"show${id}"}
  }

  def update(id: Int) = {
    complete{s"update$id"}
  }

  def edit(id: Int) = {
    complete{s"edit$id"}
  }

  def delete(id: Int)= {
    complete{s"delete$id"}
  }

  def create(model: Model2) = {
    complete{s"create ${model.title}"}
  }

  def fresh = {
    complete{"fresh"}
  }
}

trait Controller3 extends BaseController {
  import HttpService._
  def index = {
    complete{"index"}
  }

  def show(id: Int) = {
    complete{s"show${id}"}
  }

  def update(id: Int) = {
    complete{s"update$id"}
  }

  def edit(id: Int) = {
    complete{s"edit$id"}
  }

  def delete(id: Int)= {
    complete{s"delete$id"}
  }

  def create(model: Model3) = {
    complete{s"create ${model.id}"}
  }

  def fresh = {
    complete{"fresh"}
  }
}


trait Controller4 extends BaseController {
  import HttpService._

  def index = {
    complete{"index"}
  }

  def show(id: Int) = {
    complete{s"show${id}"}
  }

  def update(id: Int) = {
    complete{s"update$id"}
  }

  def edit(id: Int) = {
    complete{s"edit$id"}
  }

  def delete(id: Int)= {
    complete{s"delete$id"}
  }

  def create(model: Model4) = {
    complete{s"create ${model.id}"}
  }

  def fresh = {
    complete{"fresh"}
  }
}



trait ResorseTestable extends Routable {

  val route =
   resourse[Controller0, Model0](exclude(), {
      pathPrefix("foo") {
        get {
          complete{"bar"}
        }
      }
     }) ~
   resourse[Controller1, Model1](exclude("index", "edit", "delete", "create", "update", "show")) ~
   resourse[Controller2, Model2]({
     get0[Controller2]("index")
   }) ~
   resourse[Controller3, Model3]

 val route1 = resourse[Controller4, Model4]

}


class ResourseTest extends FunSpec with Matchers with ScalatestRouteTest with ResorseTestable {
  def actorRefFactory = system

  describe("resourse") {
    it ("when exclude is empty") {
      Get("/model0/index") ~> route ~> check{
        responseAs[String] should startWith("index")
      }
      Get("/model0/1/edit") ~> route ~> check{
        responseAs[String] should startWith("edit1")
      }
      Get("/model0/1/show") ~> route ~> check{
        responseAs[String] should startWith("show1")
      }
      Put("/model0/1/update") ~> route ~> check{
        responseAs[String] should startWith("update1")
      }
      Delete("/model0/1/delete") ~> route ~> check{
        responseAs[String] should startWith("delete1")
      }
      Get("/model0/new") ~> route ~> check{
        responseAs[String] should startWith("fresh")
      }
      Post("/model0/create", FormData(Seq("id" -> "10", "title" -> "spray"))) ~> route ~> check{
        responseAs[String] should startWith(s"create 10 - spray")
      }
    }

    it ("when exclude non empty") {
      Get("/model1/index") ~> route ~> check{
        handled should be(false)
      }
      Get("/model1/1/edit") ~> route ~> check{
        handled should be(false)
      }
      Get("/model1/1/show") ~> route ~> check{
        handled should be(false)
      }
      Put("/model1/1/update") ~> route ~> check{
        handled should be(false)
      }
      Delete("/model1/1/delete") ~> route ~> check{
        handled should be(false)
      }
      Post("/model1/create", FormData(Seq("id" -> "10", "title" -> "spray"))) ~> route ~> check{
        handled should be(false)
      }
      Get("/model1/new") ~> route ~> check{
        responseAs[String] should startWith("fresh")
      }
    }

    it("only with block") {
      Get("/model2/index") ~> route ~> check{
        responseAs[String] should startWith("index")
      }
      Get("/model2/1/edit") ~> route ~> check{
        responseAs[String] should startWith("edit1")
      }
      Get("/model2/1/show") ~> route ~> check{
        responseAs[String] should startWith("show1")
      }
      Put("/model2/1/update") ~> route ~> check{
        responseAs[String] should startWith("update1")
      }
      Delete("/model2/1/delete") ~> route ~> check{
        responseAs[String] should startWith("delete1")
      }
      Get("/model2/new") ~> route ~> check{
        responseAs[String] should startWith("fresh")
      }
      Post("/model2/create", FormData(Seq("title" -> "spray"))) ~> route ~> check{
        responseAs[String] should startWith(s"create spray")
      }
    }

    it("without block and exclude option") {
      Get("/model3/index") ~> route ~> check{
        responseAs[String] should startWith("index")
      }
      Get("/model3/1/edit") ~> route ~> check{
        responseAs[String] should startWith("edit1")
      }
      Get("/model3/1/show") ~> route ~> check{
        responseAs[String] should startWith("show1")
      }
      Put("/model3/1/update") ~> route ~> check{
        responseAs[String] should startWith("update1")
      }
      Delete("/model3/1/delete") ~> route ~> check{
        responseAs[String] should startWith("delete1")
      }
      Get("/model3/new") ~> route ~> check{
        responseAs[String] should startWith("fresh")
      }
      Post("/model3/create", FormData(Seq("id" -> "3"))) ~> route ~> check{
        responseAs[String] should startWith(s"create 3")
      }
    }

    it("nested blocks for controller0/model0") {
      Get("/model0/foo") ~> route ~> check {
        responseAs[String] should startWith("bar")
      }
    }

    it("nested blocks for controller2/model2") {
      Get("/model2/index") ~> route ~> check {
        responseAs[String] should startWith("index")
      }
    }
  }
}








