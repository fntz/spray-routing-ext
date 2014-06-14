package com.github.fntzr.spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._

  /**
   *  Contain a route helpers
   *
   */
trait HttpHelpers {
  /**
   * Take a method. And provide tranformation into route. By default use spray.http.HttpMethods.GET for request.
   * {{{
   *   match0[Contoller]("index")
   * }}}
   * Will be transform:
   * {{{
   *   pathPrefix("index") {
   *     get {
   *       controller.index
   *     }
   *   }
   * }}}
   * @param action information about method. This method will be use as route path.
   * @tparam C - you controller type
   * @return Route
   */
  def match0[C](action: String)                                                  = macro HttpHelpersImpl.match01[C]

  /**
   * Take a method. And provide transformation into route.
   * {{{
   *   match0[Controller]("index", List(GET, DELETE))
   * }}}
   *  Will be transform:
   *  {{{
   *    pathPrefix("index") {
   *      get {
   *        controller.index
   *      } ~
   *      delete {
   *        controller.index
   *      }
   *    }
   *  }}}
   * @param action - use as method from controller and path for request.
   * @param via - a list with http methods for handle request.
   * @tparam C: you controller type
   * @return Route
   */
  def match0[C](action: String, via: List[HttpMethod])                           = macro HttpHelpersImpl.match02[C]

  /**
   * Take a path with method for handle request. By default use spray.http.HttpMethods.GET for http method.
   * {{{
   *   match0[Controller](("show" / IntNumber) ~> "my_show")
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *     get {
   *       controller.my_show(num)
   *     }
   *   }
   * }}}
   * @param tuple: [[(PathMatcher[_ <: HList], String)]] path with method for handle request.
   * @tparam C - you controller
   * @return Route
   */
  def match0[C](tuple: (PathMatcher[_ <: HList], String))                        = macro HttpHelpersImpl.match03[C]

  /** Take a path with method for handle request, with list of http methods.
   *
   * {{{
   *   match0[Controller](("show" / IntNumber) ~> "my_method", List(GET, DELETE))
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *     get {
   *       controller.my_method(num)
   *     } ~
   *     delete {
   *       controller.my_method(num)
   *     }
   *   }
   * }}}
   *
   * @param tuple path with method for handle request.
   * @param via - a list with http methods for handle request.
   * @tparam C - you controller type
   * @return Route
   */
  def match0[C](tuple: (PathMatcher[_ <: HList], String), via: List[HttpMethod]) = macro HttpHelpersImpl.match0Impl[C]

  /** A root path
   *  {{{
   *    root[Controller]("root")
   *  }}}
   *  transform to
   *  {{{
   *    pathPrefix("") { controller.root }
   *  }}}
   * @param action - a controller method.
   * @tparam C - you controller type
   * @return Route
   */
  def root[C](action: String)           = macro HttpHelpersImpl.rootImpl[C]

  /** Method for create
   *
   * {{{
   *   scope("scope") {
   *     pathPrefix("foo") {
   *       get { complete("wow") }
   *   }
   * }}}
   *
   * transform to
   * {{{
   *   pathPrefix("scope") {
   *     //you block
   *   }
   * }}}
   * @param path string for path
   * @param block - block of routes
   * @return Route
   */
  def scope(path: String)(block: Route) = macro HttpHelpersImpl.scopeImpl
}

/**
 * Object with macro implementation for [[spray.routing.ext.HttpHelpers]]
 *
 */
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

    val route = q"""
      get0[${c.weakTypeOf[C]}]("/" ~> $action)
    """
    c.Expr[Route](route)
  }

  def match01[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._
    val via = q"List(spray.http.HttpMethods.GET)"
    c.Expr[Route](q"""match0[${c.weakTypeOf[C]}]($action ~> $action, $via)""")
  }

  def match02[C: c.WeakTypeTag](c: Context)(action: c.Expr[String], via: c.Expr[List[HttpMethod]]): c.Expr[Route] = {
    import c.universe._
    c.Expr[Route](q"""match0[${c.weakTypeOf[C]}]($action ~> $action, $via)""")
  }

  def match03[C: c.WeakTypeTag](c: Context)(tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    import c.universe._

    val via = q"List(spray.http.HttpMethods.GET)"
    c.Expr[Route](q"""match0[${c.weakTypeOf[C]}]($tuple, $via)""")
  }

  def match0Impl[C: c.WeakTypeTag](c: Context)
                                  (tuple: c.Expr[(PathMatcher[_ <: HList], String)], via: c.Expr[List[HttpMethod]]): c.Expr[Route] = {
    import c.universe._

    val block = via.tree.collect {
      case Select(Select(Select(Ident(_), _), _), x) =>
        val httpMethod = newTermName(s"$x".toLowerCase+"0")
        val impl = c.weakTypeOf[HttpMethods].declaration(httpMethod)
        q"$impl[${c.weakTypeOf[C]}]($tuple)"
    }.reduce((a, b) => q"$a ~ $b" )

    val route = q"""
        $block
    """

    c.Expr[Route](route)
  }
}
