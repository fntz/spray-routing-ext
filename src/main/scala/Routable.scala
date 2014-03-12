
import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._

trait Routable extends HttpService with HttpMethods with HttpHelpers with Helpers {
  def resourse[C, M](configs: (String, List[String])*)                                = macro RoutableImpl.resourseImpl[C, M]
  def resourse[C, M]                                                                  = macro RoutableImpl.resourseImpl0[C, M]
  def resourse[C, M](block: Route)                                                    = macro RoutableImpl.resourseImplWithBlock[C, M]
  def resourse[C, M](configs: (String, List[String])*)(block: Route)                  = macro RoutableImpl.resourseImplWithBlockAndConfig[C, M]
}

object RoutableImpl {
  import spray.routing.Route

  def resourseImplWithBlockAndConfig[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
                                    (configs: c.Expr[(String, List[String])]*)(block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val startPath = s"${c.weakTypeOf[M].typeSymbol.name.toString.toLowerCase}"

    val params = c.weakTypeOf[M].declarations.collect {
      case x: MethodSymbol if x.isConstructor =>
        x.paramss.map(_.map(_.asTerm))
    }.flatMap(_.flatten)

    if (params.exists(_.isParamWithDefault)) {
      c.warning(c.enclosingPosition, s"Class `${c.weakTypeOf[M]}` have parameter with default!")
    }

    val list = configs.toList.collect {
      case x => x.tree.collect {
        case Apply(_, List(Literal(Constant(x)), Apply(_, y))) =>
          val z = y.collect {
            case Literal(Constant(x)) => x
          }
          (x, z)
      }
    }.flatten.toList

    val only = list.filter{ x => x._1 == "only" }
      .map(_._2)
      .foldLeft(List[Any]())(_ ++ _)

    val exclude = list.filter{ x => x._1 == "exlcude" }
      .map(_._2)
      .foldLeft(List[Any]())(_ ++ _)

    val paramNames = params.map(_.name.toString).map(Symbol(_))
    val extract = paramNames.zip(params.map(_.typeSignature)).map{
      case (s, t) =>
        if (t.<:<(typeOf[Option[_]]))
          q"${s}.?"
        else
          q"${s}.as[$t]"
    }.toList

    val model = newTermName(s"${c.weakTypeOf[M].typeSymbol.name}")
    val controller = c.weakTypeOf[C]

    val show   = q"""get0[$controller](IntNumber ~> "show")"""
    val index  = q"get { complete { controller.index } }"
    val edit   = q"""get0[$controller]((IntNumber / "edit") ~> "edit")"""
    val update = q"""put0[$controller](IntNumber ~> "update")"""
    val delete = q"""delete0[$controller](IntNumber ~> "delete")"""
    val create = q"""
      post {
          formFields(..$extract).as($model) { (model) =>
            complete(controller.create(model))
          }
        }
    """
    val fresh = q"""get0[$controller]("new" ~> "fresh")"""

    val originalActions = List(
      ("edit", edit), ("show", show), ("update", update), ("delete", delete), ("new", fresh), ("create", create), ("index", index)
    )

    val onlyActions = originalActions.filter{
      x => only.contains(x._1)
    }
    val excludeActions = originalActions.filter {
      x => exclude.contains(x._1)
    }

    val resultRoute = (if (!onlyActions.isEmpty) {
      onlyActions
    } else {
      originalActions diff excludeActions
    }).map(_._2)

    //edit, show, update, delete, new, create, index
    val route = if (resultRoute.isEmpty) {
      q"""
        pathPrefix($startPath) {
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
          }
          $block
        }
       """
    } else {
      val actions = resultRoute.reduce((a,b) => q"$a ~ $b")
      q"""
        pathPrefix($startPath) {
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
            $actions
          }
          $block
        }
       """
    }
    c.Expr[Route](route)
  }

  def resourseImplWithBlock[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)(block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._
    c.Expr[Route](q"""resourse[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}]()($block)""")
  }

  def resourseImpl0[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context): c.Expr[Route] = {
    import c.universe._
    val r = q"""("only", List("index", "show", "create", "edit", "update", "delete", "new"))"""
    c.Expr[Route](q"""resourse[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}]($r)""")
  }

  def resourseImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)(configs: c.Expr[(String, List[String])]*): c.Expr[Route] = {
    import c.universe._
    c.Expr[Route](q"""resourse[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}](..$configs){}""")
  }
}
