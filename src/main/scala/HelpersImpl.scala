package spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context
import shapeless.HList
import spray.routing.PathMatcher


  /**
   * Provides implicits for [[String]] and [[spray.routing.PathMatcher]]
   * {{{
   *   get0[Controller]("index" ~> "my_index_method")
   *   get0[Controller](("show" / IntNumer) ~> "my_show_method")
   * }}}
   */
trait Helpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
}

  /**
   *  Containt macros for [[spray.routing.ext.Helpers]]
   */
object HelpersImpl {
    /**
     * @param c - Context
     * @param action - c.Expr[String]
     * @return [[(PathMatcher[_ <: HList], String)]]
     */
  def aliasImpl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }
}
