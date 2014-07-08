package com.github.fntzr.spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.Context

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._

/**
 * Base trait, which add form to model serialization
 */

trait HttpFormSupport {
  def postForm[C, M](action: String, exclude: String*) = macro HttpFormSupportImpl.postForm0[C, M]

  /**
   * Take an action which use as route path. ANd excluded arguments, when excluded is present,
   * then form transform to Map[String, Any], otherwise create a new Model instance.
   * {{{
   *
   *          case class Model(id: Int, title: Option[String] = None)
   *          trait HttpFormController extends BaseController {
   *            import HttpService._
   *
   *            def post(m: Model10) = {
   *              //got a Model(10, Some("spray"))
   *            }
   *
   *            def another(m: Map[String, Any]) = {
   *              //got a Map("title" -> "spray")
   *            }
   *          }
   *
   *          postForm[HttpFormController, Model10]("another", "id") ~
   *          postForm[HttpFormController, Model10]("post" ~> "post")
   *          Post("/another", FormData(Seq("title" -> "spray")))
   *          Post("/post", FormData(Seq("id" -> "10", "title" -> "spray")))
   * }}}
   * @param tuple use as route and as method for controller
   * @param exclude excluded arguments
   * @tparam C - your controller
   * @tparam M - you model
   * @return Route
   */
  def postForm[C, M](tuple: (PathMatcher[_ <: HList], String), exclude: String*) = macro HttpFormSupportImpl.postForm1[C, M]

}

private [ext] object HttpFormSupportImpl {

  def postForm0[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
               (action: c.Expr[String], exclude: c.Expr[String]*): c.Expr[Route] = {
    import c.universe._

    val route = q"""postForm[${c.weakTypeOf[C]}, ${c.weakTypeOf[M]}]($action ~> $action, ..$exclude)"""
    c.Expr[Route](route)
  }

  def postForm1[C: c.WeakTypeTag, M: c.WeakTypeTag](c: Context)
               (tuple: c.Expr[(PathMatcher[_ <: HList], String)], exclude: c.Expr[String]*): c.Expr[Route] = {
    import c.universe._


    val excl = exclude.collect {
        case Expr(Literal(Constant(x))) => x
      }


    val method = HelpersImpl.methodFromTuple[C](c)(tuple)
    val pm     = HelpersImpl.pathFromTuple[C](c)(tuple)

    val model = c.weakTypeOf[M]
    val modelName = newTermName(s"${model.typeSymbol.name}")

    if (!model.typeSymbol.isClass)
      c.abort(c.enclosingPosition, "Model must be a Class")


    val params = model.declarations.collect {
      case x: MethodSymbol if x.isConstructor =>
        x.paramss.map(_.map(_.asTerm))
    }.flatMap(_.flatten)

    if (params.size == 0) {
      c.abort(c.enclosingPosition, "Model class, must have a contrcutor values")
    }

    //without excluded arguments
    val actual = params.zip(params.map(_.typeSignature))
      .filter{case (s, t) => !excl.contains(s.name.decoded)}

    //it's used for `formFields` and extract all arguments as 'arg0? arg1.as[Int] ...
    val extract = actual.map{
        case (s, t) =>
            val z = s"${s.asTerm.name}"

            if (t.<:<(typeOf[Option[_]]))
              q"""scala.Symbol(${z}).?"""
            else
              q"""scala.Symbol(${z}).as[$t]"""
    }.toList

    val (sum: List[ValDef], names: List[Ident]) = HelpersImpl.extractValuesFromOuterMethod(c)

    val anonClassName = newTypeName(c.fresh("Controller"))

    //vals for create passed values from formFields { (vals) =>
    val vals = (0 until extract.size).map{
      case _ => ValDef(Modifiers(Flag.PARAM), newTermName(c.fresh("param")), TypeTree(), EmptyTree)
    }.toList

    //args for create arguments for create model instance: `new model(args)` | List(Ident(newTermName("a1")))
    val args = vals.map {
      case q"$mods val $tname: $tpt = $expr" => Ident(tname)
    }

    val formFields = if (excl.size == 0) {
      //then is a model instance
      q"""
        formFields(..$extract) { (..$vals) =>
          controller.$method(new $model(..$args))
        }
      """
    } else {
      //map
      val mapName = newTermName(c.fresh())

      val map = q" val $mapName = scala.collection.mutable.Map[String, Any]() "

      //actual present current collection of keys, vals present passed values
      //Ident(newTermName("key")), newTermName("$minus$greater")), List(Ident(newTermName("value"))
      val mapKeys = actual.zip(args).map {
        case _ @ (_ @ (a, _), b) =>
          val key = s"${a.asTerm.name}"
          q"$mapName += ($key -> $b)"
      }

      //Return immutable map
      val immutableMapName = newTermName(c.fresh())
      val imMap = q" val $immutableMapName = scala.collection.immutable.Map[String, Any]() ++ $mapName "

      q"""
        formFields(..$extract) { (..$vals) =>
          $map
          ..$mapKeys
          $imMap
          controller.$method($immutableMapName)
        }
      """
    }

    //List(ValDef(Modifiers(PARAM), newTermName("title"), TypeTree(), EmptyTree)
    val result = q"""
        pathPrefix($pm) {
          requestInstance { request0 =>
            case class $anonClassName(..$sum) extends ${c.weakTypeOf[C]}
            val controller = new $anonClassName(..$names)
            $formFields
          }
        }
    """

  //var result = q"""pathPrefix("/"){post{complete{"das"}}}"""
    c.Expr[Route](result)
  }


}

