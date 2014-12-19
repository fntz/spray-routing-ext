package com.github.fntzr.spray.routing.ext.annotations

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

import spray.routing._
import spray.http._
import shapeless._
import shapeless.Traversables._

/**
 * {{{
 *
 *   trait Controller {
 *
 *     @get("/") def index = ....
 *
 *     @get("/foo") def fooMethod = ...
 *
 *     @get("/bar") def barMethod(s: String, i: Int) = ...
 *
 *   }
 *
 *   // will be expand to some like this
 *   val controller = new Controller()
 *   get('/') { controller.index() } ~
 *   get('/foo') { controller.fooMethod() } ~
 *   get('/bar' / Segment / IntNumber) { (s, i) => controller.barMethod(s,i) }
 *
 *
 * }}}
 */
class get(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro RouteImpl.getImpl
}

class post(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ???
}

class put(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ???
}

class delete(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ???
}

class scope(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ???
}

class root(path: String) extends StaticAnnotation {
  //? get("/")
  def macroTransform(annottees: Any*) = macro ???
}

trait RouteBuild {
  def buildRoute[T] = macro RouteImpl.buildImpl[T]
}


private [annotations] object RouteImpl {

  def buildImpl[T: c.WeakTypeTag](c: Context): c.Expr[Unit] = {
    import c.universe._

    println(123)

    c.Expr[Unit](q"")
  }


  // need extract:
  // + path
  // + method name
  // + type of arguments for method
  // + enclosing trait
  //
  // stop compile,
  // + when path not string
  // + when annotation not for Def
  // + when Def have a TypeParams
  //
  def getImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val path = c.prefix.tree match {
      case Apply(_, List(Literal(Constant(x)))) => x
    }

    if (path.getClass.getName != "java.lang.String") {
      c.error(c.enclosingPosition, s"path must be String, but '${path.getClass.getName}' given.")
    }

    val results = annottees.map(_.tree).toList.map {
      case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
        if (!tparams.isEmpty) {
          c.error(c.enclosingPosition, s"TypeParams not supported for method which mark for route")
        }
        (tname, paramss.flatten) // => (name, List(val z: String = _))
      case _ => c.error(c.enclosingPosition, s"Route annotation only work for methods")
    }

    //TODO check tparams, and parents
    val traitName = c.enclosingClass match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        tpname
      case _ =>
        c.error(c.enclosingClass.pos, s"Use route annotation only with trait")
    }

    //need convert method arguments to PathMatcher
    

    c.Expr[Unit](q"")
  }

  def postImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    println(c.prefix)
    println(annottees.map(_.tree).toList)

    c.Expr[Unit](q"")
  }

  def putImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    println(c.prefix)
    println(annottees.map(_.tree).toList)

    c.Expr[Unit](q"")
  }

  def deleteImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    println(c.prefix)
    println(annottees.map(_.tree).toList)

    c.Expr[Unit](q"")
  }

  def scopeImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    println(c.prefix)
    println(annottees.map(_.tree).toList)

    c.Expr[Unit](q"")
  }

  def rootImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    println(c.prefix)
    println(annottees.map(_.tree).toList)

    c.Expr[Unit](q"")
  }


}





