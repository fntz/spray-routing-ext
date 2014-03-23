package spray.routing.ext

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
   * @return [[spray.http.HttpRequest]]
   */
  def request: spray.http.HttpRequest
}


