import spray.routing.ext._

case class Post(id: Int, title: String, description: String)

trait PostController extends BaseController {

}

trait ApplicationRoute extends Routable {
  val route = resourse[PostController, Post]
}


