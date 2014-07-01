package com.github.fntzr.spray.routing.ext

import spray.http.MediaType
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import spray.routing.Route
import spray.httpx.marshalling.ToResponseMarshallable
/** A BaseController trait
 *
 *  It a top hierarchy controllers. You can extend own controller
 *  with this trait.
 *  {{{
 *    trait MyController extends BaseController {
 *      //you actions ...
 *    }
 *  }}}
 *
 *  Trait contain request method, which contatin information about
 *  current request. You might use in controller actions:
 *  {{{
 *    trait MyController extends BaseController {
 *      def index = {
 *       if (request.method == GET) {
 *         //do something
 *       } else {
 *         //do otherwise
 *       }
 *     }
 *    }
 *  }}}
 *
 *
 */
trait BaseController {
  /**
   * When controller created, it method contain current request object.
   *
   * @return spray.http.HttpRequest
   */
  def request: spray.http.HttpRequest
}

trait RespondToSupport { //: StandardRoute
  /**
   * With this method, possibly create a simple response by current `Accept header`
   * @example
   *          trait MyController extends BaseController with RespondToSupport {
   *            import HttpService._ // !!!important, because macro expand with methods from this namespace
   *            def index = {
   *              respondTo {
   *                case `text/html` => "html response"
   *                case `application/json` if this_ajax_request => "json response"
   *              }
   *            }
   *          }
   *
   *          //And then request with Accept: `text/html` we get a "html response"
   *          //And when request with Accept: `application/json` and wit ajax, we get a "json response"
   *          //Otherwise get Error: 400 Bad Request
   *
   * Underhood this transform to next code:
   * @example
   *          val header = request.headers.find(_.name == "Accept") //request from BaseController
   *          header match {
   *             case Some(header) =>
   *                val h = header.value
   *                h match {
   *                  case value if h.contains(`media/type`.value) && you_guard =>
   *                    respondWithMediaType(`media/type`) {
   *                      complete { you_response } // from previous: it's a "html response" or "json response"
   *                    }
   *                  //other cases
   *                  case _ => reject(...)
   *                }
   *             case None => reject(...)
   *          }
   *
   * @param pf: PartialFunction[MediaType, ToResponseMarshallable] a case block
   * @return Route
   */
  def respondTo(pf: PartialFunction[MediaType, ToResponseMarshallable]): Route = macro RespondMacro.respondImpl
}

object RespondMacro {
  def respondImpl(c: Context)(pf: c.Expr[PartialFunction[MediaType, ToResponseMarshallable]]): c.Expr[Route] = {
    import c.universe._

    val name = newTermName(c.fresh())
    val h = newTermName(c.fresh())
    val x = newTermName(c.fresh())

    val result = pf.tree.collect {
      case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" if s"$tname" == "applyOrElse" =>
        expr match {
          case q"$expr match { case ..$cases }" =>
            //The last element, is a default: case (defaultCase$ @ _) => default.apply(x1)
            val cs = cases.slice(0, cases.size - 1)
            cs.collect {
              case CaseDef(mType, guard, tree) => (mType, guard, tree)
            }
        }
    }
    val r = result.flatten

    val allowMediaTypes = (r.collect {
      case (t, _, _) if s"$t" != "_" => s"$t"
    }).mkString(",")

    val msg = s"Only the following media types are supported: $allowMediaTypes"


    val default = r.collect {
      case (mType, guard, tree) if s"$mType" == "_" => q"""reject(MalformedHeaderRejection("Accept", $msg))"""
    }

    if (default.size == 0) {
      c.error(c.enclosingPosition, "Specify default branch as `case _ => 'result'`")
    }

    val resultDefault = default(0)


    val cases = r.collect {
      case (mType, guard, tree) if s"$mType" != "_" =>
        val g = if (guard == EmptyTree) {
          q"true"
        } else {
          guard
        }
        cq"""$x if $g && $h.contains($mType.value) => respondWithMediaType($mType) { complete{$tree} }"""
      case (mType, _, tree) if s"$mType" == "_" =>
        cq"""_ => reject(MalformedHeaderRejection("Accept", $msg))"""
    }


    val zz = q"""
       val $name = request.headers.find(_.name == "Accept")
       $name match {
          case Some(header) =>
            val $h = header.value
            $h match { case ..$cases }
          case None =>
            $resultDefault
        }
    """

    c.Expr[Route](zz)
  }
}

