
import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._
//import spray.http.MediaTypes._

trait Routable extends HttpService with Helpers {
  import spray.routing.Route

  import HttpMethods._

  def get0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.get0Impl[C]
  def post0[C](tuple: (PathMatcher[_ <: HList], String))   = macro RoutableImpl.post0Impl[C]
  def put0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.put0Impl[C]
  def delete0[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.delete0Impl[C]

  def match0[C](tuple: (PathMatcher[_ <: HList], String), via: List[HttpMethod]) = macro RoutableImpl.match0Impl[C]

  def root[C](action: String) = macro RoutableImpl.rootImpl[C]

  def resourse[C, M](configs: (String, List[String])*) = macro RoutableImpl.resourseImpl[C, M]
  def resourse[C, M] = macro RoutableImpl.resourseImpl0[C, M]

  def scope(path: String)(block: Route) = macro RoutableImpl.scopeImpl
}


object RoutableImpl {
  import spray.routing.Route

  def scopeImpl(c: Context)(path: c.Expr[String])(block: c.Expr[Route]): c.Expr[Route] = {
    import c.universe._

    val route = q"""
      pathPrefix($path) {
        $block
      }
    """

    c.Expr[Route](route)
  }

  def resourseImpl0[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context): c.Expr[Route] = {
    import c.universe._
    val r = q"""("only", List("index", "show", "create", "edit", "update", "delete", "new"))"""
    resourseImpl[C, M](c)(c.Expr[(String, List[String])](r))
  }

  def resourseImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)(configs: c.Expr[(String, List[String])]*): c.Expr[Route] = {
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

    val show = q"""get0[$controller](IntNumber ~> "show")"""
    val index = q"get { complete { controller.index } }"
    val edit = q"""get0[$controller]((IntNumber / "edit") ~> "edit")"""
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

    //TODO: requestInstance
    //edit, show, update, delete, new, create, index
    val route = if (resultRoute.isEmpty) {
      q"""
        pathPrefix($startPath) {
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
          }
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
        }
       """
    }
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

  //FIXME: possible bug when route as "route" ~> "action" ?
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

    //FIXME: rewrite this
    var count = 0
    if (pm.children.size == 1) {
      count = 1
    } else {
      pm.tpe.foreach {
        case x  =>
          if (x.termSymbol.isPackage && x.toString.contains("scala")) count += 1
      }
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
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
            $httpMethod {
              complete{ controller.$method(..$vals) }
            }
          }
        }
      """
    } else {
      q"""
        pathPrefix($pm) {
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
            $httpMethod {
              complete{ controller.$method }
            }
          }
        }
      """
    }

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
