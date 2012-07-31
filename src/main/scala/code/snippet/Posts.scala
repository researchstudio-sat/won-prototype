package code.snippet

import net.liftweb.util.CssSel
import code.lib.YabeHelper
import code.lib.ModelBinder._
import code.model.{Need, User}
import net.liftweb.mapper.{By, Ascending, OrderBy, Like}
import code.service.UserService
import net.liftweb.util.Helpers._

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 31.07.12
 * Time: 15:49
 * To change this template use File | Settings | File Templates.
 */

class Posts {


  def list: CssSel = {
    val posts = getPosts()
    var odd = "even"
    val x = posts.map {
      u =>
        odd = YabeHelper.oddOrEven(odd);
        ".post_item" #> bindModel(u, { "tr [class]" #> odd}) _
    }
    "#posts" #> x

  }

  private def getPosts() = {
    // ToDo: change from Need to Post
    val userId: Long = UserService.isUserLoggedIn.openOr(-1)
    Need.findAll(By(Need.userID, userId))
    //Need.findAll
  }
}
