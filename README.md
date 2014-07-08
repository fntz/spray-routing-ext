Extension for create a Rails like routes for http://spray.io/

My post about: [spray-routing-ext: Extend spray-routing](http://fntzr.github.io/scala/2014/04/20/spray-routing-ext-extend-spray-routing.html)

Api doc: [doc](http://fntzr.github.io/api/#com.github.fntzr.spray.routing.ext.Routable)

Example: [blog](https://github.com/fntzr/spray-routing-ext/blob/master/sample/src/main/scala/example.scala)

Library add a `resourse, match0, get0, post0, delete0, put0, root` route directives.


Install
------------------------------------

In `Build.scala`

```scala
"com.github.fntzr"  %% "spray-routing-ext" % "0.2"  // For scala 2.10
"com.github.fntzr"  %% "spray-routing-ext" % "0.3"  // For scala 2.11
```

Methods
-------------------

### Include and use

```scala
import com.github.fntzr.spray.routing.ext._

trait ApplicationRouteService extends Routable {
  def route = ....
}
```



### Simple http methods

In package defined a `get0`, `post0`, `delete0`, and `put0` methods for handle request by Get, Post, Delete or Put http method.

```scala
  //routes
  get0[Controller]("path") ~                                          // GET http://example.com/path 
  get0[Controller]("path-name" ~> "controllerAction") ~               // GET http://example.com/path-name
  delete0[Controller](("delete-path" / IntNumber) ~> "anotherAction") // DELETE http://example.com/delete-path/1
  
  //in controller:
  
  trait Controller {
    def path = { }
    
    def controllerAction = { ... }
                       ^^^^
                      without arguments
    
    def anotherAction(num: Int) = { ... }
                     ^^^^^^^^^^
                       IntNumber        
  }     
```

### Helper methods for routing

This is a `match0`, `root` and `scope` methods. 

+ `math0` - used for define routes for few methods together
+ `root`  - just alias for `get0[Controller]("/" ~> "action")
+ `scope` - define route `path` with nested block, all url paths will be start with `path`

```scala 
  match0[Controller]("path-name") ~                                   // GET example.com/path-name
  match0[Controller]("another-path" ~> "action", List(GET, DELETE) ~  // GET | DELETE example.com/another-path
  root[Controller]("action")                                          // GET example.com/
  scope("scope") {
    get0[Controller]("get")                                           // GET example.com/get
  }
```

### Resourse

In Rails framework `resourse` it's a set routes (`index`, `show`, `edit`, `create`, `update`, `delete`, `new`) which operate a `Model`.
All urls will be started with `/model/`. Example:

```scala

case class Model(title: String, description: String)

trait Controller {
  def index =  ...
  def show(title: String) = ...
  def edit(title: String) = ...
  def create(model: Model) = ...
  def update(title: String) = ...
  def delete(title: String) = ...
  def fresh = ... // it's a `new` path
}

//route
  
  resourse[Controller, Model](Segment)
                             ^^^^^^^^^
                          define subroute, by default subroute is a IntNumber     
  
```

This code generate urls:

```
GET    example.com/model/index  
GET    example.com/model/post-title
GET    example.com/model/post-title/edit
POST   example.com/model/
PUT    example.com/model/post-title
DELETE example.com/model/post-title
GET    example.com/model/new 
```

In `resourse` you might exclude unnecessary methods, or define nested block, and you might define own [separator](http://spray.io/documentation/1.1-SNAPSHOT/api/index.html#spray.routing.PathMatchers)

[More](https://github.com/fntzr/spray-routing-ext/blob/master/src%2Ftest%2Fscala%2FRoutableTest.scala#L154) [resourses](https://github.com/fntzr/spray-routing-ext/blob/master/src%2Ftest%2Fscala%2FSpecifySubrouteTest.scala#L71)

#### Note: For Controller define method `fresh` instead of `new`, because in scala, `new` reserved keyworld.


### Controllers

Controllers it's only scala traits. When spray-routing-ext generate code inherited from you Controller, and you might use in a controller 
predefined value - `request: spray.http.HttpRequest`, but you might explicitly extends `BaseController`, which contain one abstract method.

```scala
trait MyController extends BaseController {
  def myAction = {
    val headers = request.headers
    ...
  }
}
```

### respondTo helper

Sometimes you need according to `Accept` header send request with right `Content-Type`

```scala
  
  def method = {
    val accept = ... // extract from request Accept header 
    
    if accept is a html
      complete ...
    else if accept is a json and request with ajax 
      complete ...
    else
      reject
  }
```

Now possible reduce the code

```scala
trait MyController extends BaseController with RespondToSupport {
  def method = {
    respondTo {
      case `text/html`    =>    <html><body>....</body></html>
      case `application/json` if isAjax  => """ { json: "ok" } """ 
    }
  }
}
```

Now, when we request content with `Accept: text/html` will be got a `<html><body>....</body></html>` response,
when we request content with `Accept: application/json` with ajax, will be got `""" { json: "ok" } """`,
otherwise will be got `BadRequest`.
 
### Form support

`spray-routing-ext` adds a `postForm` method:

```scala
case class Model(id: Int, title: String)

postForm[Controller, Model]("action")
postForm[Controller, Model]("post-path" ~> "anotherAction", exclude("id")

// in controller

trait Controller {
  
  def action(model: Model) = ... 
            ^^^^^^^^^^^^^^
            got a Model instance
  
  def anotherAction(map: Map[String, Any]) = ....
                   ^^^^^^^^^^^^^^^^^^^^^^^^
                   got a Map, because we excluded `id` attribute 
}
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

### DI

Often need a pass into controllers some values (bd connection, actor path, predefined values...)

It's a simple:

```scala

trait RouteService extends Routable {
  def route(db: ActorRef, render: Render) =  { // method take a `db` connection and `renderer`
    resourse[PostController, Post]
  }
}

class MyService extends Actor with RouteService {
  val db = ...
  val render = ....
  def receive = runRoute(route(db, render))
                        ^^^^^^^^^^^^^^^^^^^^^
                          pass `db` connection and `renderer`
                               
}

trait Injection {
  val db: ActorRef
  val render: Render
}

//then into PostController 

trait PostController extends BaseController with RespondToSupport with Injection {
                                                                      ^^^^^^^^^^^^
   ....                                                                     
}
```

When application srart, in PostController possible use a `db` and `render`.



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
