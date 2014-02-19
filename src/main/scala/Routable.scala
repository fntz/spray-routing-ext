
import scala.language.experimental.macros
import scala.reflect.macros.Context

import shapeless.Nat._0
import spray.http.Uri.Path.Segment
import spray.routing._
import spray.http._
import MediaTypes._
import shapeless._
import shapeless.Traversables._
import spray.http.Uri.Path

trait RHelpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.string2Impl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro RoutableImpl.path2Impl
  }
}

trait Routable extends HttpService with RHelpers {
  import spray.routing.Route

  import HttpMethods._
  def custom[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.customImpl[C]
  def get0[C](tuple: (PathMatcher[_ <: HList], String)) = macro RoutableImpl.get0Impl[C]
  def post0 = ???
  def put0 = ???
  def delete0 = ???

  def match0[C]() = ???

  def root = ???

  def resourse = ???
}

object RoutableImpl {
  import spray.routing.Route

  def string2Impl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def path2Impl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def get0Impl[C: c.WeakTypeTag](c: Context)
              (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    import c.universe._

    val (_, pm, action) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled
    val z = pm.tpe

    //FIXME: rewrite this
    var count = 0
    z.foreach {
      case x  =>
        if (x.termSymbol.isPackage && x.toString.contains("scala")) count += 1
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

    val route = if (count != 0) {
      q"""
        pathPrefix($pm) { ..$paramVals =>
          val controller = new ${c.weakTypeOf[C]}{}
          get {
            complete{ controller.$method(..$vals) }
          }
        }
      """
    } else {
      q"""
        pathPrefix($pm) {
          val controller = new ${c.weakTypeOf[C]}{}
          get {
            complete{ controller.$method }
          }
        }
      """
    }

    c.Expr[Route](route)
  }

  def customImpl[C: c.WeakTypeTag](c: Context)
                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    import c.universe._

    val (_, pm, action) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled

    println(pm)

    val route = q"""
      path("/") {
        get {
          complete("1")
        }
      }
    """
    c.Expr[Route](route)
  }


  def match0Impl[C: c.WeakTypeTag](c: Context) = {

  }
}
/*
TypeRef(
  ThisType(spray.routing),
  spray.routing.PathMatcher,
  List(
    TypeRef(
      ThisType(shapeless),
      shapeless.$colon$colon,
      List(
        TypeRef(
          ThisType(scala), scala.Int, List()
        ),
        TypeRef(
          ThisType(shapeless),
          shapeless.HNil,
          List()
        )
      )
    )
  )
)

*/
/*
TypeRef(
  ThisType(spray.routing),
  spray.routing.PathMatcher,
  List(
    TypeRef(
      ThisType(shapeless),
      shapeless.$colon$colon,
      List(
        TypeRef(
          ThisType(scala), scala.Int, List()
        ),
        TypeRef(
          ThisType(shapeless), shapeless.$colon$colon,
          List(
            TypeRef(
              SingleType(
                ThisType(scala), scala.Predef
              ),
              newTypeName("String"),
              List()
            ),
            TypeRef(
              ThisType(shapeless), shapeless.HNil, List()
            )
          )
        )
      )
    )
  )
)
*/
/*
Apply(
  TypeApply(
    Select(
      Select(
        This(newTypeName("routing")),
        spray.routing.Prepender
      ),
      newTermName("apply")
    ),
    List(TypeTree(), TypeTree(), TypeTree())
  ),
  List(
    Apply(
      TypeApply(
        Select(
          Select(This(newTypeName("shapeless")), shapeless.PrependAux),
          newTermName("hlistPrepend")
        ),
        List(TypeTree(), TypeTree(), TypeTree(), TypeTree())
      ),
      List(
        TypeApply(
          Select(
            Select(
              This(newTypeName("shapeless")), shapeless.PrependAux),
            newTermName("hnilPrepend")
          ),
          List(TypeTree())
        )
      )
    )
  )
)


Apply(
  TypeApply(
    Select(
      Select(
        This(newTypeName("routing")), spray.routing.Prepender
      ),
      newTermName("apply")
    ),
    List(TypeTree(), TypeTree(), TypeTree())
  ),
  List(
    TypeApply(
      Select(
        Select(
          This(newTypeName("shapeless")),
          shapeless.PrependAux
        ),
        newTermName("hnilPrepend")
      ),
      List(TypeTree())
    )
  )
)


*/
