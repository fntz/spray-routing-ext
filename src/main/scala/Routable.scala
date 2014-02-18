
import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.http.Uri.Path.Segment
import spray.routing._
import spray.http._
import MediaTypes._
import shapeless._
import shapeless.Traversables._
import spray.http.Uri.Path

trait RHelpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.string2Impl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.path2Impl
  }
}

trait Routable extends HttpService with RHelpers {
  import spray.routing.Route

  import HttpMethods._
  def custom[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.customImpl[C]
  def get0 = ???
  def post0 = ???
  def put0 = ???
  def delete0 = ???

  def match0[C]() = ???

  def root = ???

  def resourse = ???
}

object RoutableImpl {
  import spray.routing.Route

  def string2Impl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def path2Impl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def customImpl[C: c.WeakTypeTag](c: Context)
                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    import c.universe._

    val (_, pm, action) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled

    println(pm)

    val route = q"""
      path("/") {
        get {
          complete("1")
        }
      }
    """
    c.Expr[Route](route)
  }

}














