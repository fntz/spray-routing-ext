package spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import scala.language.implicitConversions


object exclude {
  def apply(xs: String*) = xs.toList
}

trait Routable extends HttpService with HttpMethods with HttpHelpers with Helpers {
  def resourse[C, M](exclude: List[String])  = macro RoutableImpl.resourse0Impl[C, M]
  def resourse[C, M](block: Route) = macro RoutableImpl.resourse1Impl[C, M]
  def resourse[C, M](exclude: List[String], block: Route) = macro RoutableImpl.resourseImpl[C, M]
  def resourse[C, M] = macro RoutableImpl.resourse4Impl[C, M]
}


object RoutableImpl {
  import spray.routing.Route

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

    val route = q"""
      pathPrefix($startPath) {
          $result
        }
    """

    c.Expr[Route](route)
  }
  def resourseImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                  (exclude: c.Expr[List[String]], block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val startPath = convertToPath(s"${c.weakTypeOf[M].typeSymbol.name.toString}")

    val result = getRoute[C, M](c)(exclude)

    val route = q"""
      pathPrefix($startPath) {
          $result ~
          $block
        }
    """

    c.Expr[Route](route)
  }


  private def convertToPath(s: String) = {
    val r = "[A-Z]".r
    (r replaceAllIn(s"${s.head.toString.toLowerCase}${s.tail}", "-$0")).toLowerCase
  }

  private def getRoute[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
              (exclude: c.Expr[List[String]]): c.Expr[Route] = {
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
    val create = q"""
      requestInstance { request0 =>
        post {
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }

            formFields(..$extract).as($model) { (model) =>
              controller.create(model)
            }
          }
      }
    """

    val fresh = q"""get0[$controller]("new" ~> "fresh")"""

    val originalActions = List(
      ("edit", edit), ("show", show), ("update", update), ("delete", delete), ("new", fresh), ("create", create), ("index", index)
    )

    val excludeActions = originalActions.filter { x => list.contains(x._1)}

    val resultRoute = (originalActions diff excludeActions) map(_._2)

    val route = resultRoute.reduce((a,b) => q"$a ~ $b")
    c.Expr[Route](q"$route")
  }

}
