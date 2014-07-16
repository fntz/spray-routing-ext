import spray.httpx.marshalling.ToResponseMarshallable

import scala.concurrent.{Future, Await}
import scala.Some
import com.github.fntzr.spray.routing.ext._
import akka.actor._
import akka.io.IO
import spray.can.Http
import spray.http.MediaTypes._
import spray.routing._
import scala.collection.mutable.HashMap
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
  import scala.concurrent.ExecutionContext.Implicits.global

  def respond(t: ToResponseMarshallable) = respondWithMediaType(`text/html`) & complete(t)

  def index = {
    respond {
      (db ? Index).mapTo[HashMap[Int, Post]].map { posts =>
        render.indexView(posts)
      }
    }
  }

  def edit(id: Int) = {
    respond {
      (db ? Show(id)).mapTo[Option[Post]].map { post =>
        render.editView(post)
      }
    }
  }

  def create = {
    formFields('title, 'description) { (title, description) =>
      onSuccess(db ? Create(title, description)) {
        case id: Int => redirect(s"/post/${id}", StatusCodes.MovedPermanently)
      }
    }
  }

  def show(id: Int) = {
    respond {
      (db ? Show(id)).mapTo[Option[Post]].map { post =>
        render.showView(post)
      }
    }
  }


  def delete(id: Int) = {
    onSuccess(db ? Delete(id)) {
      case isDelete: Boolean => redirect("/", StatusCodes.MovedPermanently)
    }
  }

  def update(id: Int) = {
    formFields('id.as[Int], 'title, 'description) { (id, title, description) =>
      val post = Post(id, title, description)
      onSuccess(db ? Update(post)) {
        case x: Boolean => redirect(s"/post/${id}", StatusCodes.MovedPermanently)
      }
    }
  }

  def fresh = {
    respond(render.freshView)
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
  val posts = new HashMap[Int, Post]
  posts += (1 -> Post(1, "new post", "description"))

  def receive = {
    case Index => sender ! posts
    case Show(id: Int) => sender ! posts.get(id)
    case Update(post: Post) =>
      posts += (post.id -> post)
      sender ! true
    case Delete(id: Int) =>
      posts.get(id) match {
        case Some(x) =>
          posts -= id
          sender ! true
        case None => sender ! false
      }
    case Create(title: String, description: String) =>
      val id = if (posts.size == 0) {
        1
      } else {
        posts.map{ case p @ (id, post) => id }.max + 1
      }
      posts += (id -> Post(id, title, description))
      sender ! id
  }
}

trait HtmlView {

  def indexView(posts: HashMap[Int, Post]) = {
    html(
      <div>
        {
          posts.collect {
            case p @ (_, Post(id: Int, title: String, description: String)) =>
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
