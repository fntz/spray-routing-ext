package spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context
import shapeless.HList
import spray.routing.PathMatcher

trait Helpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
    def :->(str: String*): (String, List[String]) = macro HelpersImpl.assocImpl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
}

object HelpersImpl {

  def aliasImpl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def assocImpl(c: Context)(str: c.Expr[String]*): c.Expr[(String, List[String])] = {
    import c.universe._

    val it = c.prefix.tree match {
      case Apply(Select(_, _), List(x)) => x
    }
    val list = str.collect {
      case Expr(x) => x
    }.toList

    val result = q"($it, List[String](..$list))"
    c.Expr[(String, List[String])](result)
  }
}
