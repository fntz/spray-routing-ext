
import scala.language.experimental.macros
import scala.reflect.macros.Context

import shapeless.Nat._0
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

  def get0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.get0Impl[C]
  def post0[C](tuple: (PathMatcher[_ <: HList], String))   = macro RoutableImpl.post0Impl[C]
  def put0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.put0Impl[C]
  def delete0[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.delete0Impl[C]

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


  def get0Impl[C: c.WeakTypeTag](c: Context)
      (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
      methodImpl[C](c)(tuple, HttpMethods.GET)
  }

  def post0Impl[C: c.WeakTypeTag](c: Context)
                                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.POST)
  }

  def put0Impl[C: c.WeakTypeTag](c: Context)
                                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.PUT)
  }

  def delete0Impl[C: c.WeakTypeTag](c: Context)
                                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.DELETE)
  }

  def methodImpl[C: c.WeakTypeTag](c: Context)
                (tuple: c.Expr[(PathMatcher[_ <: HList], String)], mth: HttpMethod): c.Expr[Route] = {
    import c.universe._

    val (_, pm, action) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled
    val z = pm.tpe

    //FIXME: rewrite this
    var count = 0
    z.foreach {
      case x  =>
        if (x.termSymbol.isPackage && x.toString.contains("scala")) count += 1
    }

    val methodName = action match {
      case Literal(Constant(x)) => x.asInstanceOf[String]
    }

    val method = c.weakTypeOf[C].declaration(newTermName(methodName))

    if (method == NoSymbol) {
      c.error(c.enclosingPosition, s"Method `$methodName` not found in `${c.weakTypeOf[C]}`")
    }

    val paramVals = (0 until count).collect{
      case x => ValDef(Modifiers(Flag.PARAM), newTermName(s"tmp$x"), TypeTree(), EmptyTree)
    }.toList

    val vals = (0 until count).collect {
      case x => Ident(newTermName(s"tmp$x"))
    }.toList

    val httpMethod = newTermName(mth.toString.toLowerCase)

    val route = if (count != 0) {
      q"""
        pathPrefix($pm) { ..$paramVals =>
          val controller = new ${c.weakTypeOf[C]}{}
          $httpMethod {
            complete{ controller.$method(..$vals) }
          }
        }
      """
    } else {
      q"""
        pathPrefix($pm) {
          val controller = new ${c.weakTypeOf[C]}{}
          $httpMethod {
            complete{ controller.$method }
          }
        }
      """
    }

    c.Expr[Route](route)
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


  def match0Impl[C: c.WeakTypeTag](c: Context) = {

  }
}
