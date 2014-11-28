package com.github.fntzr.spray.routing.ext

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

/*
private [ext] object RespondMacro {
  def respondImpl(c: Context)(pf: c.Expr[PartialFunction[MediaType, ToResponseMarshallable]]): c.Expr[Route] = {
    import c.universe._

    val name = TermName(c.freshName())
    val h = TermName(c.freshName())
    val x = TermName(c.freshName())

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

*/