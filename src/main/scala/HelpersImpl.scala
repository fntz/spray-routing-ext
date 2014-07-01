package com.github.fntzr.spray.routing.ext

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import spray.routing.PathMatcher
import shapeless._
import shapeless.Traversables._

/**
 * Provides implicits for String and spray.routing.PathMatcher
 * {{{
 *   get0[Controller]("index" ~> "my_index_method")
 *   get0[Controller](("show" / IntNumer) ~> "my_show_method")
 * }}}
 */
trait Helpers {
  implicit class S2A(path: String) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
  implicit class PM2A(pm: PathMatcher[_ <: HList]) {
    def ~>(action: String):(PathMatcher[_ <: HList], String) = macro HelpersImpl.aliasImpl
  }
}

  /**
   *  Containt macros for [[spray.routing.ext.Helpers]]
   */
object HelpersImpl {

  def aliasImpl(c: Context)(action: c.Expr[String]): c.Expr[(PathMatcher[_ <: HList], String)] = {
    import c.universe._

    val pm = c.prefix.tree.children.toList(1)
    val t = q"($pm, $action)"
    c.Expr[(PathMatcher[_ <: HList], String)](t)
  }

    /**
     * When route define in method:
     *   def myMethod(a: Int, s: String) = {
     *      resourse[C, M]
     *   }
     *
     *  This methods return all arguments from outer method (a, s) + define request0 value, also return all names for params
      * @param c [Context]
     * @return Tuple2[List[ValDef], List[Ident]]
     */
  def extractValuesFromOuterMethod(c: Context) = {
    import c.universe._
    val requestVal = List(q"val request: spray.http.HttpRequest")
    val outerMethod = c.enclosingMethod
    val (sum: List[ValDef], names: List[Ident]) = if (outerMethod != null) {

      val vs = (outerMethod match {
        case DefDef(_, _, _, List(List(xs @ _*)), _, _) => xs
      }).asInstanceOf[List[ValDef]]

      val vvs = vs.map{case x: ValDef => q"val ${x.name}:${x.tpt}"}

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
    val (_, _, action) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled

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
    import c.universe._

    val (_, pm, _) = tuple.tree.children.toHList[Tree::Tree::Tree::HNil].get.tupled
    pm
  }
}
