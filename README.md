Extension for create a Rails like routes for http://spray.io/

My post about: [spray-routing-ext: Extend spray-routing](http://fntzr.github.io/scala/2014/04/20/spray-routing-ext-extend-spray-routing.html)

Api doc: [doc](http://fntzr.github.io/api/#com.github.fntzr.spray.routing.ext.Routable)

Example: [blog](https://github.com/fntzr/spray-routing-ext/blob/master/sample/src/main/scala/example.scala)

Library add a `resourse, match0, get0, post0, delete0, put0, root` route directives.


Install (by now only for scala 2.10)
------------------------------------

In `Build.scala`

```scala
"com.github.fntzr"  %% "spray-routing-ext" % "0.1" 
```

Usage
-------

### Define routes


```scala
import com.github.fntzr.spray-routing-ext._

trait MyRoutes extends Routable {
  val route = resourse[PostController, Post] {
    pathPrefixt("foo") {
      get {
        complete{"foo"}
      }
    }
  } ~ get0[OtherController]("route") ~
  post0[Controller0](("foo" / IntNumber) ~> "action") ~
  match0[Controller0]("bar", List(GET, POST, PUT)) ~
  resourse[ModelController, Model] ~
  root[PostController]("index")
}
```

### Define controllers


```scala
trait PostController extends BaseController {
  def index = {
    complete("index")
  }
  def show(id: Int) = {
    complete(s"show:$id")
  }
  //others...
}
```

TODO
-----

* postForm, getForm ... anyMethodForm\[Controller, Model\](fields)
* add default value for show\delete\edit methods, now is `id`


License
--------

Copyright (c) 2014 fntzr <fantazuor@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
