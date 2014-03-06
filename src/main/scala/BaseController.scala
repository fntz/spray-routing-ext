
import spray.routing.HttpService

trait BaseController extends HttpService {
  def request: spray.http.HttpRequest
  def format:  List[spray.http.MediaRange]
}
