package spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._

/** Trait containt http methods realisation
 *
 */
trait HttpMethods {

  /**
   * Take an action which use as route path.
   *
   * @param action use as route and as method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def get0[C](action: String)    = macro HttpMethodsImpl.get01Impl[C]

  /**
   * Take an action which use as route path.
   *
   * @param action  use as route and as method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def post0[C](action: String)   = macro HttpMethodsImpl.post01Impl[C]

  /**
   * Take an action which use as route path.
   *
   * @param action use as route and as method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def put0[C](action: String)    = macro HttpMethodsImpl.put01Impl[C]

  /**
   * Take an action which use as route path.
   *
   * @param action use as route and as method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def delete0[C](action: String) = macro HttpMethodsImpl.delete01Impl[C]

  /**
   * Take a tuple with path and method for controller
   * {{{
   *   get0[Controller]( ("show" / IntNumber) ~> "my_method")
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *     get {
   *       controller.my_method(num)
   *     }
   *   }
   * }}}
   * @param tuple path and method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def get0[C](tuple: (PathMatcher[_ <: HList], String))    = macro HttpMethodsImpl.get0Impl[C]

  /**
   * Take a tuple with path and method for controller
   * {{{
   *   post0[Controller]( ("show" / IntNumber) ~> "my_method")
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *     post {
   *       controller.my_method(num)
   *     }
   *   }
   * }}}
   * @param tuple path and method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def post0[C](tuple: (PathMatcher[_ <: HList], String))   = macro HttpMethodsImpl.post0Impl[C]

  /**
   * Take a tuple with path and method for controller
   * {{{
   *   put0[Controller]( ("show" / IntNumber) ~> "my_method")
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *    put {
   *       controller.my_method(num)
   *     }
   *   }
   * }}}
   * Note: With this method created post method, which take a hidden field _method, and processing request
   * @param tuple path and method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def put0[C](tuple: (PathMatcher[_ <: HList], String))    = macro HttpMethodsImpl.put0Impl[C]

  /**
   * Take a tuple with path and method for controller
   * {{{
   *   delete0[Controller]( ("show" / IntNumber) ~> "my_method")
   * }}}
   * transform to
   * {{{
   *   pathPrefix("show" / IntNumber) { num =>
   *     delete {
   *       controller.my_method(num)
   *     }
   *   }
   * }}}
   * Note: With this method created post method, which take a hidden field _method, and processing request
   * @param tuple path and method for controller
   * @tparam C - your controller
   * @return [[Route]]
   */
  def delete0[C](tuple: (PathMatcher[_ <: HList], String)) = macro HttpMethodsImpl.delete0Impl[C]
}

/**
 * Object with http methods implementation.
 *
 */
object HttpMethodsImpl {

  def get01Impl[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._

    val route = q"""get0[${c.weakTypeOf[C]}]($action ~> $action)"""
    c.Expr[Route](route)
  }

  def post01Impl[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._

    val route = q"""post0[${c.weakTypeOf[C]}]($action ~> $action)"""
    c.Expr[Route](route)
  }

  def put01Impl[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._

    val route = q"""put0[${c.weakTypeOf[C]}]($action ~> $action)"""
    c.Expr[Route](route)
  }

  def delete01Impl[C: c.WeakTypeTag](c: Context)(action: c.Expr[String]): c.Expr[Route] = {
    import c.universe._

    val route = q"""delete0[${c.weakTypeOf[C]}]($action ~> $action)"""
    c.Expr[Route](route)
  }

  def get0Impl[C: c.WeakTypeTag](c: Context)
                                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.GET, HttpMethods.GET)
  }

  def post0Impl[C: c.WeakTypeTag](c: Context)
                                 (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.POST, HttpMethods.POST)
  }

  def put0Impl[C: c.WeakTypeTag](c: Context)
                                (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.PUT, HttpMethods.POST)
  }

  def delete0Impl[C: c.WeakTypeTag](c: Context)
                                   (tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Expr[Route] = {
    methodImpl[C](c)(tuple, HttpMethods.DELETE, HttpMethods.POST)
  }

  def methodImpl[C: c.WeakTypeTag](c: Context)
                (tuple: c.Expr[(PathMatcher[_ <: HList], String)], mth: HttpMethod, emulateWith: HttpMethod): c.Expr[Route] = {
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

    val emulate = emulateWith.toString.toLowerCase
    val plain = mth.toString.toLowerCase
    val httpMethod    = newTermName(plain)
    val emulateMethod = newTermName(emulate)


    val complete = if (count != 0) {
      q"controller.$method(..$vals)"
    } else {
      q"controller.$method"
    }

    val mainRoute = q"""
      $httpMethod {
        $complete
      }
    """

    val result = if (mth != emulateWith) {
      q"""
        $mainRoute ~
        $emulateMethod {
          anyParam('_method.?) { method: Option[String] =>
            method match {
              case Some(x) if x == $plain =>
                $complete
              case _ =>
                failWith(new spray.http.IllegalRequestException(spray.http.StatusCodes.BadRequest))
            }
          }
        }
      """
    } else {
      mainRoute
    }

    val route = if (count != 0) {
      q"""
        pathPrefix($pm) { ..$paramVals =>
          requestInstance { request0 =>
            val controller = new ${c.weakTypeOf[C]}{
              def request = request0
            }
            $result
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
            $result
          }
        }
      """
    }

    c.Expr[Route](route)
  }
}