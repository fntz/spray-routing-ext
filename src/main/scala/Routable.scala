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
def resource[C, M](exclude: List[String])  = macro RoutableImpl.resource0Impl[C, M]
def resource[C, M](block: Route) = macro RoutableImpl.resource1Impl[C, M]
def resource[C, M](exclude: List[String], block: Route) = macro RoutableImpl.resourceImpl[C, M]
def resource[C, M] = macro RoutableImpl.resource4Impl[C, M]

subroute:

def resource[C, M](exclude: List[String], sub: PathMatcher1[_])
def resource[C, M](sub: PathMatcher1[_], block: Route)
def resource[C, M](exclude: List[String], sub: PathMatcher1[_], block: Route)

 */


/**
 * Trait contatin methods for resource implementation.
 *  With resource method you might quick create routes for you controller. Also map form information onto Model.
 *  {{{
 *    resource[Controller, Model]
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
   *   resource[Controller, Model](exclude("index", "show", "new"))
   * }}}
   * @param exclude - list with excluded methods (index, show, ...)
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resource[C, M](exclude: List[String])  = macro RoutableImpl.resource0Impl[C, M]

  /** Define routes with nested block
   *  {{{
   *    resource[Controller, Model] {
   *      get0[Controller]("other")
   *    }
   *  }}}
   * @param block Route - block with nested routes
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resource[C, M](block: Route) = macro RoutableImpl.resource1Impl[C, M]

  /** Define routes without excluded actions, and nested block
   *  {{{
   *    resource[Controller, Model](exclude("index"), {
   *      get0[Controller]("other")
   *    })
   *  }}}
   * @param exclude - excluded actions
   * @param block Route - nested block
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resource[C, M](exclude: List[String], block: Route) = macro RoutableImpl.resourceImpl[C, M]

  /** Simple define routes
   * {{{
   *   resource[Controller, Model]
   * }}}
   * @tparam C - you controller
   * @tparam M - you model
   * @return Route
   */
  def resource[C, M] = macro RoutableImpl.resource4Impl[C, M]


  /**
   * Define resource with specified subroute
   *
   * {{{
   *   resource[Controller, Model](exclude("show"), Segment)
   * }}}
   *
   * @param exclude  - list of excluded methods
   * @param sub      - part of route, which will be pass into controller method, and use for create a url
   * @tparam C - Controller
   * @tparam M - Model
   * @return Route
   */
  def resource[C, M](exclude: List[String], sub: PathMatcher1[_]): Route = macro RoutableImpl.resource5Impl[C, M]

  /**
   * Define resource with subroute and nexted block
   *
   * {{{
   *   resource[Controller, Model](IntNumber, {
   *     pathPrefix("/path") {
   *        get { complete { "path" } }
   *     }
   *  }
   * }}}
   *
   * @param sub - part of route, which will be pass into controller method
   * @param block - nested route block, and use for create a url
   * @tparam C - Controller
   * @tparam M - Model
   * @return Route
   */
  def resource[C, M](sub: PathMatcher1[_], block: Route): Route = macro RoutableImpl.resource6Impl[C, M]

  /**
   * Define resource with subroute, block and excluded methods.
   *
   * {{{
   *   resource[Controller, Method](exclude("show"), {
   *     pathPrefix("/path") {
   *       complete{ "path" }
   *     }
   *   },
   *   IntNumber)
   * }}}
   *
   * @param exclude - list of excluded methods
   * @param block - nested route block
   * @param sub - part of route, which will be pass into controller method, and use for create a url
   * @tparam C
   * @tparam M
   * @return
   */
  def resource[C, M](exclude: List[String], block: Route, sub: PathMatcher1[_]): Route = macro RoutableImpl.resource7Impl[C, M]

}

/** Object, which contatain resource implementation.
 *
 */
private [ext] object RoutableImpl {
  import spray.routing.Route

  def resource7Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]], block: c.Expr[Route], sub: c.Expr[PathMatcher1[_]]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude, sub)

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


  def resource6Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (sub: c.Expr[PathMatcher1[_]], block: c.Expr[Route]): c.Expr[Route] = {

    import c.universe._

    val route = q"""resource[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](List[String](), $block, $sub)"""

    c.Expr[Route](route)
  }


  def resource5Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]], sub: c.Expr[PathMatcher1[_]]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude, sub)

    if (result.isEmpty) {
      c.error(c.enclosingPosition, s"resource should have a Route type")
    }

    val route = q"""
      pathPrefix($startPath) {
        ${result.get}
      }
    """

    c.Expr[Route](route)
  }


  def resource4Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context): c.Expr[Route] = {
    import c.universe._

    val route = q"""resource[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](List[String]())"""
    c.Expr[Route](route)
  }

  def resource1Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val route = q"""resource[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](List[String](), $block)"""
    c.Expr[Route](route)
  }

  def resource0Impl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                   (exclude: c.Expr[List[String]]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude, c.Expr[PathMatcher1[_]](q"IntNumber"))

    if (result.isEmpty) {
      c.error(c.enclosingPosition, s"resource should have a Route type")
    }

    val route = q"""
      pathPrefix($startPath) {
        ${result.get}
      }
    """

    c.Expr[Route](route)
  }
  def resourceImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                  (exclude: c.Expr[List[String]], block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude, c.Expr[PathMatcher1[_]](q"IntNumber"))

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
              (exclude: c.Expr[List[String]], sub: c.Expr[PathMatcher1[_]]):Option[c.Expr[Route]] = {
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

    val show   = q"""get0[$controller]($sub ~> "show")"""
    val index  = q"""get0[$controller]("index")"""
    val edit   = q"""get0[$controller](($sub / "edit") ~> "edit")"""
    val update = q"""put0[$controller]($sub ~> "update")"""
    val delete = q"""delete0[$controller]($sub ~> "delete")"""


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










