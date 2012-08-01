package code.snippet

import net.liftweb.util.CssSel
import code.lib.YabeHelper
import code.lib.ModelBinder._
import code.model.{Post, User}
import net.liftweb.mapper._
import code.service.UserService
import net.liftweb.util.Helpers._
import net.liftweb.http.{CurrentReq, SHtml, S}

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
      u:Post =>
        odd = YabeHelper.oddOrEven(odd);
        ".post_item" #> bindModel(u, { "tr [class]" #> odd}) _
    }
    "#posts" #> x

  }

  private def getPosts() = {
    val userId: Long = UserService.isUserLoggedIn.openOr(-1)
    Post.findAll(By(Post.userID, userId))
  }

  def titlePostsCreated: CssSel = {
    "*" #> <title>
      {("Post successfully created. ")}
    </title>
  }


  def edit: CssSel = {
    val id = S.param("id").openTheBox
    val post = Post.find(By(Post.id, id.toLong)).openTheBox

    def process() = {
      post.validate match {
        case Nil => {
          post.save
          S.redirectTo("/success/" + post.id + "/" + YabeHelper.generateAdminHash(post.id))
        }
        case errors => S.error(errors)
      }
    }

    "#post-edit" #> bindModel(post) _ &
      "name=tags" #> SHtml.text(post.tags.concat, post.tags.setMultiple(post, _)) &
      "type=submit" #> SHtml.onSubmitUnit(process)

  }

  def getUserName: CssSel = {
    val firstName = User.currentUser.openTheBox.firstName
    val lastName = User.currentUser.openTheBox.lastName

    "span" #> (firstName + " " + lastName)
  }


  def getSuccess: CssSel = {
    val id = S.param("id").openTheBox
    val post = Post.find(By(Post.id, id.toLong)).openTheBox
    val webApplicationPort = CurrentReq.value.request.serverPort
    var webApplicationUrl = CurrentReq.value.request.serverName

    if(webApplicationPort != 80){
      webApplicationUrl = webApplicationUrl + ":" + webApplicationPort
    }

    val adminLink = "http://" + webApplicationUrl + "/administration/" + id + "/" + YabeHelper.generateAdminHash(post.id)
    val shareLink = "http://" + webApplicationUrl + "/read/" + id + "/" + YabeHelper.generateHash(post.id)

    "#postTitle *" #> (post.title) &
      "#postTitle [href]" #> (shareLink) &
      "#adminQuickLink [href]" #> (adminLink) &
      "#shareLink *" #> (shareLink) &
      "#adminLink *" #> (adminLink)

  }
}
