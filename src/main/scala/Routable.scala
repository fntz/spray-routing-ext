
import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._
import spray.routing.PathMatchers.IntNumber

trait RHelpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.aliasImpl
    def :->(str: String*): (String, List[String]) = macro RoutableImpl.assocImpl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.aliasImpl
  }
}

trait Routable extends HttpService with RHelpers {
  import spray.routing.Route

  import HttpMethods._

  def get0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.get0Impl[C]
  def post0[C](tuple: (PathMatcher[_ <: HList], String))   = macro RoutableImpl.post0Impl[C]
  def put0[C](tuple: (PathMatcher[_ <: HList], String))    = macro RoutableImpl.put0Impl[C]
  def delete0[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.delete0Impl[C]

  def match0[C](tuple: (PathMatcher[_ <: HList], String), via: List[HttpMethod]) = macro RoutableImpl.match0Impl[C]

  def root[C](action: String) = macro RoutableImpl.rootImpl[C]

  def resourse[C, M] = macro RoutableImpl.resourseImpl[C, M]

}


object RoutableImpl {
  import spray.routing.Route

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



  def resourseImpl[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context): c.Expr[Route] = {
    import c.universe._

    val startPath = s"${c.weakTypeOf[M].typeSymbol.name.toString.toLowerCase}"

    val params = c.weakTypeOf[M].declarations.collect {
      case x: MethodSymbol if x.isConstructor =>
        x.paramss.map(_.map(_.asTerm))
    }.flatMap(_.flatten)

    if (params.exists(_.isParamWithDefault)) {
      c.warning(c.enclosingPosition, s"Class `${c.weakTypeOf[M]}` have parameter with default!")
    }

    val paramNames = params.map(_.name.toString).map(Symbol(_))
    val extract = paramNames.zip(params.map(_.typeSignature)).map{
        case (s, t) =>
          if (t.<:<(typeOf[Option[_]]))
            q"${s}.?"
          else
            q"${s}.as[$t]"
    }.toList

    val model = newTermName(s"${c.weakTypeOf[M].typeSymbol.name}")

    val route = q"""
      pathPrefix($startPath) {
        val controller = new ${c.weakTypeOf[C]}{}
        pathPrefix(IntNumber) { num =>
          pathPrefix("edit") {
            get { complete { controller.edit(num) } }
          } ~
          get { complete { controller.show(num) } } ~
          put { complete { controller.update(num) } } ~
          delete { complete { controller.delete(num) } }
        } ~
        pathPrefix("new") {
          get {
            complete { controller.fresh }
          }
        } ~
        post {
          formFields(..$extract).as($model) { (model) =>
              complete(controller.create(model))
          }
        } ~
        get {
          complete { controller.index }
        }
      }
    """

    c.Expr[Route](route)
  }

  def aliasImpl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
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
        val controller = new ${c.weakTypeOf[C]}{}
        get {
          complete{controller.$method}
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
/*
Apply(TypeApply(Select(Apply(_, List(x)), _), _), List(y))
Apply(
  TypeApply(
    Select(
      Apply(
        TypeApply(
          Select(
            Select(
              This(newTypeName("scala")), scala.Predef
            ),
            newTermName("any2ArrowAssoc")
          ),
          List(TypeTree()
          )
        ),
        List(Literal(Constant("a")))
      ),
      newTermName("$minus$greater")
    ),
    List(TypeTree())
  ),
  List(
    Literal(Constant(1))
  )
)
*/