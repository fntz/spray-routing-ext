
import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._

trait HttpHelpers {
  def match0[C](tuple: (PathMatcher[_ <: HList], String), via: List[HttpMethod]) = macro HttpHelpersImpl.match0Impl[C]
  def root[C](action: String) = macro HttpHelpersImpl.rootImpl[C]
  def scope(path: String)(block: Route) = macro HttpHelpersImpl.scopeImpl
}

object HttpHelpersImpl {
  def scopeImpl(c: Context)(path: c.Expr[String])(block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val route = q"""
      pathPrefix($path) {
        $block
      }
    """

    c.Expr[Route](route)
  }

  def rootImpl[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._

    val methodName = action.tree match {
      case Literal(Constant(x)) => s"$x"
    }

    val method = c.weakTypeOf[C].declaration(newTermName(methodName))

    if (method == NoSymbol) {
      c.error(c.enclosingPosition, s"Method `$methodName` not found in `${c.weakTypeOf[C]}`")
    }

    val route = q"""
      path("") {
        requestInstance { request0 =>
          val controller = new ${c.weakTypeOf[C]}{
            def request = request0
          }
          get {
            complete{controller.$method}
          }
        }
      }
    """
    c.Expr[Route](route)
  }

  def match0Impl[C: c.WeakTypeTag](c: Context)
                                  (tuple: c.Expr[(PathMatcher[_ <: HList], String)], via: c.Expr[List[HttpMethod]]): c.Expr[Route] = {
    import c.universe._
    //TODO: check Routable type in current compilation unit
    val block = via.tree.collect {
      case Select(Select(Select(Ident(_), _), _), x) =>
        val httpMethod = newTermName(s"$x".toLowerCase+"0")
        val impl = c.weakTypeOf[Routable].declaration(httpMethod)
        q"$impl[${c.weakTypeOf[C]}]($tuple)"
    }.reduce((a, b) => q"$a ~ $b" )

    val route = q"""
        $block
    """

    c.Expr[Route](route)
  }
}
