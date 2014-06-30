package com.github.fntzr.spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import scala.language.implicitConversions

import spray.routing.PathMatchers._
import spray.routing._
import spray.routing.PathMatcher1
/**
 * Object for transformation String* into List
 * {{{
 *   exclude("index")
 * }}}
 */
object exclude {
  def apply(xs: String*) = xs.toList
}

/*
def resourse[C, M](exclude: List[String])  = macro RoutableImpl.resourse0Impl[C, M]
def resourse[C, M](block: Route) = macro RoutableImpl.resourse1Impl[C, M]
def resourse[C, M](exclude: List[String], block: Route) = macro RoutableImpl.resourseImpl[C, M]
def resourse[C, M] = macro RoutableImpl.resourse4Impl[C, M]

subroute:

def resourse[C, M](exclude: List[String], sub: PathMatcher1[_])
def resourse[C, M](sub: PathMatcher1[_], block: Route)
def resourse[C, M](exclude: List[String], sub: PathMatcher1[_], block: Route)

 */


/**
 * Trait contatin methods for resourse implementation.
 *  With resourse method you might quick create routes for you controller. Also map form information onto Model.
 *  {{{
 *    resourse[Controller, Model]
 *  }}}
 *   transform to
 *  {{{
 *    pathPrefix("model") {
 *      //methods for controller.index
 *      // controller.show
 *      // ...
 *    }
 *   }}}
 * Note: for `new` method in controller use `fresh` name.
 */
trait Routable extends HttpService with HttpMethods with HttpHelpers with Helpers with HttpFormSupport {

  /** Define routes without excluded pathes.
   * {{{
   *   resourse[Controller, Model](exclude("index", "show", "new"))
   * }}}
   * @param exclude - list with excluded methods (index, show, ...)
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resourse[C, M](exclude: List[String])  = macro RoutableImpl.resourse0Impl[C, M]

  /** Define routes with nested block
   *  {{{
   *    resourse[Controller, Model] {
   *      get0[Controller]("other")
   *    }
   *  }}}
   * @param block Route - block with nested routes
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resourse[C, M](block: Route) = macro RoutableImpl.resourse1Impl[C, M]

  /** Define routes without excluded actions, and nested block
   *  {{{
   *    resourse[Controller, Model](exclude("index"), {
   *      get0[Controller]("other")
   *    })
   *  }}}
   * @param exclude - excluded actions
   * @param block Route - nested block
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resourse[C, M](exclude: List[String], block: Route) = macro RoutableImpl.resourseImpl[C, M]

  /** Simple define routes
   * {{{
   *   resourse[Controller, Model]
   * }}}
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resourse[C, M] = macro RoutableImpl.resourse4Impl[C, M]


  def resourse[C, M](exclude: List[String], sub: PathMatcher1[_]) = macro RoutableImpl.resourse5Impl[C, M]
  def resourse[C, M](sub: PathMatcher1[_], block: Route) = macro RoutableImpl.resourse6Impl[C, M]
  def resourse[C, M](exclude: List[String], block: Route, sub: PathMatcher1[_]) = macro RoutableImpl.resourse7Impl[C, M]


}

/** Object, which contatain resourse implementation.
 *
 */
object RoutableImpl {
  import spray.routing.Route

  def resourse7Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]], block: c.Expr[Route], sub: c.Expr[PathMatcher1[_]]) = ???

  def resourse6Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (sub: c.Expr[PathMatcher1[_]], block: c.Expr[Route]) = ???

  def resourse5Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]], sub: c.Expr[PathMatcher1[_]]) = ???


  def resourse4Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context): c.Expr[Route] = {
    import c.universe._

    val route = q"""resourse[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](List[String]())"""
    c.Expr[Route](route)
  }

  def resourse1Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val route = q"""resourse[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](List[String](), $block)"""
    c.Expr[Route](route)
  }

  def resourse0Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude)

    val route = result match {
      case Some(x) =>
        q"""
          pathPrefix($startPath) {
            $x
          }
        """
      case None =>
        c.error(c.enclosingPosition, s"resourse should have a Route type")
        q""" get{complete{"123"}} """
    }

    c.Expr[Route](route)
  }
  def resourseImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                  (exclude: c.Expr[List[String]], block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude)

    val route = result match {
      case Some(x) =>
        q"""
          pathPrefix($startPath) {
            $x ~
            $block
          }
        """
      case None =>
        q"""
          pathPrefix($startPath) {
            $block
          }
        """
    }


    c.Expr[Route](route)
  }


  private def convertToPath(s: String) = {
    val r = "[A-Z]".r
    (r replaceAllIn(s"${s.head.toString.toLowerCase}${s.tail}", "-$0")).toLowerCase
  }

  private def getRoute[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
              (exclude: c.Expr[List[String]]):Option[c.Expr[Route]] = {
    import c.universe._

    val params = c.weakTypeOf[M].declarations.collect {
      case x: MethodSymbol if x.isConstructor =>
        x.paramss.map(_.map(_.asTerm))
    }.flatMap(_.flatten)

    if (params.exists(_.isParamWithDefault)) {
      c.warning(c.enclosingPosition, s"Class `${c.weakTypeOf[M]}` have parameter with default!")
    }

    val list = exclude.tree.collect {
      case Literal(Constant(x)) => s"$x"
    }.toList

    val paramNames = params.map(_.name.toString).map(Symbol(_))
    val extract = paramNames.zip(params.map(_.typeSignature)).map{
      case (s, t) =>
        if (t.<:<(typeOf[Option[_]]))
          q"${s}.?"
        else
          q"${s}.as[$t]"
    }.toList

    if (extract.isEmpty) {
      c.abort(c.enclosingPosition, s"Model `${c.weakTypeOf[M]}` should have a parameters!")
    }

    val model = newTermName(s"${c.weakTypeOf[M].typeSymbol.name}")
    val controller = c.weakTypeOf[C]

    val show   = q"""get0[$controller](IntNumber ~> "show")"""
    val index  = q"""get0[$controller]("index")"""
    val edit   = q"""get0[$controller]((IntNumber / "edit") ~> "edit")"""
    val update = q"""put0[$controller](IntNumber ~> "update")"""
    val delete = q"""delete0[$controller](IntNumber ~> "delete")"""


    val (sum: List[ValDef], names: List[Ident]) = HelpersImpl.extractValuesFromOuterMethod(c)

    val anonClassName = newTypeName(c.fresh("Controller"))
    val create = q"""
      post {
        requestInstance { request0 =>
          case class $anonClassName(..$sum) extends ${c.weakTypeOf[C]}
          val controller = new $anonClassName(..$names)

          formFields(..$extract).as($model) { (model) =>
            controller.create(model)
          }
        }
      }
    """

    val fresh = q"""get0[$controller]("new" ~> "fresh")"""

    val original0 = List(
      ("new", fresh), ("index", index), ("create", create)
    )

    val original1 = List(
      ("edit", edit), ("show", show), ("update", update), ("delete", delete)
    )

    val exclude0 = original0.filter { x => list.contains(x._1)}
    val exclude1 = original1.filter { x => list.contains(x._1)}

    val resultForBlock = (original1 diff exclude1) map(_._2)
    val resultOutBlock = (original0 diff exclude0) map(_._2)

    val route0 = if(resultForBlock.isEmpty) {
      None
    } else {
      val sum = resultForBlock.reduce((a,b) => q"$a ~ $b")
      Some(q"""
        overrideMethodWithParameter("_method") {
          $sum
        }
      """)
    }

    val route1 = if(resultOutBlock.isEmpty) {
      None
    } else {
      Some(resultOutBlock.reduce((a,b) => q"$a ~ $b"))
    }

    val route = (route0, route1) match {
      case (Some(a), Some(b)) => Some(c.Expr[Route](q"$a ~ $b"))
      case (Some(a), None) => Some(c.Expr[Route](q"$a"))
      case (None, Some(a)) => Some(c.Expr[Route](q"$a"))
      case (None, None) => None
    }

    route
  }

}










