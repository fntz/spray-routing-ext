package com.github.fntzr.spray.routing.ext

import shapeless._
import spray.routing.PathMatcher

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Provides implicits for String and spray.routing.PathMatcher
 * {{{
 *   get0[Controller]("index" ~> "my_index_method")
 *   get0[Controller](("show" / IntNumer) ~> "my_show_method")
 * }}}
 */
trait Helpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.alias0Impl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
}

  /**
   *  Containt macros for [[spray.routing.ext.Helpers]]
   */
private [ext] object HelpersImpl {

  def alias0Impl(c: Context)(action: c.Expr[String]) = {
    import c.universe._

    val segment = c.prefix.tree.children.toList(1)
    val pm = c.prefix.tree.children.toList(1)
    val z = pm match {
      case Literal(Constant(x)) => x.asInstanceOf[String]
    }

    val t = q"""(PathMatcher.segmentStringToPathMatcher($z), $action)"""
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

  def aliasImpl(c: Context)(action: c.Expr[String]) = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)

    val t = q"($pm, $action)"

    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

    /**
     * When route define in method:
     *   def myMethod(a: Int, s: String) = {
     *      resource[C, M]
     *   }
     *
     *  This methods return all arguments from outer method (a, s) + define request0 value, also return all names for params
      * @param c [Context]
     * @return Tuple2[List[ValDef], List[Ident]]
     */
  def extractValuesFromOuterMethod(c: Context) = {
    import c.universe._
    val requestVal = List(q"val request: spray.http.HttpRequest")
    val outerMethod = c.internal.enclosingOwner

    val (sum: List[ValDef], names: List[Ident]) = if (outerMethod.isMethod) {

      val vs = outerMethod.asMethod.paramLists.flatten

      val vvs = vs.map{x => q"val ${x.asTerm.name}:${x.typeSignature}"}

      val sum = requestVal ++ vvs

      val tmpNames = List("request0") ++ vs.map{x => s"${x.name}"}

      val names = tmpNames.collect{ case x =>Ident(TermName(x))}
      (sum, names)
    } else {
      val sum = requestVal

      val tmpNames = List("request0")

      val names = tmpNames.collect{ case x =>Ident(TermName(x))}
      (sum, names)
    }
    (sum, names)
  }

    /**
     *  From given tuple Tuple2.apply[ResorseTestable.this.IntNumber.type, String](ResorseTestable.this.IntNumber, "show")
     *  and class C, extract method symbol for class, when method not defined, this stop compilation
     *
     */

  def methodFromTuple[C: c.WeakTypeTag](c: Context)(tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.universe.Symbol = {
    import c.universe._

    /*
     Tuple2.apply[ResorseTestable.this.IntNumber.type, String](ResorseTestable.this.IntNumber, "show")
     Tuple2.apply[ResorseTestable.this.IntNumber.type, String](ResorseTestable.this.IntNumber, "update")
     Tuple2.apply[ResorseTestable.this.IntNumber.type, String](ResorseTestable.this.IntNumber, "delete")
    */
    val action = tuple.tree.children(2)

    val methodName = action match {
      case Literal(Constant(x)) => x.asInstanceOf[String]
    }

    val method = c.weakTypeOf[C].decl(TermName(methodName))

    if (method == NoSymbol) {
      c.error(c.enclosingPosition, s"Method `$methodName` not found in `${c.weakTypeOf[C]}`")
    }
    method
  }

    /**
     *  Extract path from tuple
     *
     */
  def pathFromTuple[C: c.WeakTypeTag](c: Context)(tuple: c.Expr[(PathMatcher[_ <: HList], String)]): c.Tree = {
    val pm = tuple.tree.children(1)
    pm
  }
}
