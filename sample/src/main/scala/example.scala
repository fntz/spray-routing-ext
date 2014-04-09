import scala.concurrent.Await
import scala.Some
import com.github.fntzr.spray.routing.ext._
import akka.actor._
import akka.io.IO
import spray.can.Http
import spray.http.MediaTypes._
import spray.routing._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import akka.pattern.{ ask, pipe }
import akka.util.Timeout
import scala.xml._
import spray.http.StatusCodes
import scala.language.postfixOps

case class Post(id: Int, title: String, description: String)

trait DBInj {
  val db: ActorRef
}

trait HtmlViewInj {
  val render: HtmlView
}

trait PostController extends BaseController with DBInj with HtmlViewInj {
  import Messages._
  import HttpService._

  implicit val timeout = Timeout(3 seconds)

  def index = {
    val posts = Await.result(db ? Index, timeout.duration).asInstanceOf[ArrayBuffer[Post]]
    respond(render.indexView(posts))
  }

  def edit(id: Int) = {
    val post = Await.result(db ? Show(id), timeout.duration).asInstanceOf[Option[Post]]
    respond(render.editView(post))
  }

  def create = {
    formFields('title, 'description) { (title, description) =>
      val future = db ? Create(title, description)
      val id = Await.result(future, timeout.duration).asInstanceOf[Int]
      val uri = s"/post/${id}"
      redirect(uri, StatusCodes.MovedPermanently)
    }
  }

  def show(id: Int) = {
    val post = Await.result(db ? Show(id), timeout.duration).asInstanceOf[Option[Post]]
    respond(render.showView(post))
  }

  def delete(id: Int) = {
    val isDelete = Await.result(db ? Delete(id), timeout.duration).asInstanceOf[Boolean]
    if (isDelete) {
      redirect("/", StatusCodes.MovedPermanently)
    } else {
      respond(<span>Error with delete post with id: {id}</span>)
    }
  }

  def update(id: Int) = {
    formFields('id.as[Int], 'title, 'description) { (id, title, description) =>
      val post = Post(id, title, description)
      val isUpdate =  Await.result(db ? Update(post), timeout.duration).asInstanceOf[Boolean]
      if (isUpdate) {
        redirect(s"/post/${id}", StatusCodes.MovedPermanently)
      } else {
        respond(<span>Error with update post with id: {id}</span>)
      }
    }
  }

  def fresh = {
    respond(render.freshView)
  }

  private def respond(html: scala.xml.Elem) = {
    respondWithMediaType(`text/html`) {
      complete {
        {html}
      }
    }
  }

}

object Messages {
  case object Index
  case class Show(id: Int)
  case class Update(post: Post)
  case class Delete(id: Int)
  case class Create(title: String, description: String)
}

class DBActor extends Actor {
  import Messages._
  val posts = new ArrayBuffer[Post]
  posts += Post(1, "new post", "description")

  def receive = {
    case Index => sender ! posts
    case Show(id: Int) => sender ! posts.find { case p => p.id == id}
    case Update(post: Post) =>
      val r = posts.find{case p: Post => p.id == post.id} match {
        case Some(p) =>
          posts -= p
          true
        case None =>
          false
      }
      if (!r) { sender ! false }
      posts += post
      sender ! true
    case Delete(id: Int) =>
      posts.find{case p => p.id == id} match {
        case Some(p) =>
          posts -= p
          sender ! true
        case None =>
          sender ! false
      }
    case Create(title: String, description: String) =>
      val id = if (posts.size == 0) {
        1
      } else {
        posts.map(_.id).max + 1
      }
      posts += Post(id, title, description)
      sender ! id
  }
}

trait HtmlView {

  def indexView(posts: ArrayBuffer[Post]) = {
    html(
      <div>
        {
        posts.collect {
          case Post(id: Int, title: String, description: String) =>
            val href = s"/post/${id}/"
            <div>
              <h3><a href={href}>{id}: {title}</a></h3>
              <p>{description}</p>
            </div>
        }
        }
        <a href="/post/new">create new post</a>
      </div>
    )
  }

  def showView(post: Option[Post]) = {
    html(
      post match {
        case Some(Post(id: Int, title: String, description: String)) =>
          val edit   = s"/post/${id}/edit"
          <div>
            <h3>{id}: {title}</h3>
            <p>{description}</p>
            <span>
              <a href={edit}>Edit</a>
              {deleteHelper(id)}
            </span>
          </div>

        case None => <span>Not found Post</span>
      }
    )
  }

  def editView(post: Option[Post]) = {
    html(
      post match {
        case p: Some[Post] =>
          val action = s"/post/${p.get.id}/?_method=put"
          <div>
            <h3>Edit form post</h3>
            {form(p, action, "update post")}
          </div>

        case None => <span>Not found Post</span>
      }
    )
  }

  def freshView = {
    html(form(None, "/post/create", "create post"))
  }

  def form(post: Option[Post], action: String, submit: String) = {
    val (id: Int, title: String, description: String) = post match {
      case Some(Post(id: Int, title: String, description: String)) => (id, title, description)
      case None => (0, "", "")
    }

    <form action={action} method="POST">
      {
      if (id != 0) {
          <input type="hidden" value={s"$id"} name="id" />
      }
      }
    Title: <input type = "text" name="title" value={title}/> <br/>
    Description: <textarea name="description">{description}</textarea> <br/>
        <input type="submit" value={submit} />
    </form>
  }

  def deleteHelper(id: Int) = {
    val delete = s"/post/${id}/?_method=delete"
    <form action={delete} method="POST">
      <input type="submit" value="delete" />
    </form>
  }

  private def html(html: scala.xml.Elem) = {
    <html>
      <body>
        {html}
      </body>
    </html>
  }
}

trait ApplicationRouteService extends Routable {
  import Blog._
  implicit def executionContext = actorRefFactory.dispatcher

  def route(db: ActorRef, render: HtmlView) =  {
    resourse[PostController, Post](exclude("create"), {
      post0[PostController]("create")
    }) ~ root[PostController]("index")
  }
}


class ServiceActor(db: ActorRef) extends Actor with ApplicationRouteService {
  def actorRefFactory = context
  def receive = runRoute(route(db, new HtmlView {}))
}

object Blog extends App {
  implicit val system = ActorSystem("blog")
  val db = system.actorOf(Props[DBActor], "db")
  val service = system.actorOf(Props(classOf[ServiceActor], db), "blog-service")
  IO(Http) ! Http.Bind(service, "localhost", port = 8080)
}
