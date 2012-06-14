package code.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._
import java.util.Date
import Helpers._
import code.model._
import code.lib._
import ModelBinder._
import net.liftweb.http._

class Needs {
  def listLatest: CssSel = {
    val latestNeed = Need.find(OrderBy(Need.id, Descending))
    //"*" #> bindModel()

    latestNeed match {
      case Full(p) => {
        "*" #> bindModel(p, {
          ".post-offers *" #> (" | " + p.countOffers + " offers " + p.latestOfferAuthor)
        }) _
      }
      case _ => "*" #> <span></span>
    }
  }

  def listOlder: CssSel = {
    val latestNeed = Need.find(OrderBy(Need.id, Descending))

    latestNeed match {
      case Full(p) => {
        val olderNeeds = Need.findAll(OrderBy(Need.id, Descending)).
          filter(p.id != _.id)

        renderNeedsList(olderNeeds)
      }
      case _ => "*" #> ""
    }
  }

  def listNeedsByTag: CssSel = {
    val posts = Need.getNeedsByTag(S.param("tag") openOr "")

    renderNeedsList(posts)
  }

  def getTag: CssSel = {
    "span" #> (S.param("tag") openOr "")
  }

  def titleNeedsByTag: CssSel = {
    "*" #> <title>
      {("Needs tagged with " + (S.param("tag") openOr ""))}
    </title>
  }

  def titleNeedsCreated: CssSel = {
    "*" #> <title>
      {("Need successfully created. ")}
    </title>
  }

  private def renderNeedsList(posts: List[Need]): CssSel = {
    "*" #> posts.map {
      p =>
        "*" #> bindModel(p, {
          ".post-offers *" #> (" | " + p.countOffers + " offers " + p.latestOfferAuthor)
        }) _
    }
  }

  def title: CssSel = {
    val post = Need.find(By(Need.id, S.param("id").openTheBox.toLong))
    post match {
      case Full(p) => "*" #> <title>
        {p.title}
      </title>
      case _ => "*" #> <title>invalid post</title>
    }
  }

  def read: CssSel = {
    val post = Need.find(By(Need.id, S.param("id").openTheBox.toLong))

    post match {
      case Full(p) => {
        "*" #> bindModel(p, {
          ".post-tags *" #> Unparsed(p.showTagMetaStr)
        }) _
      }
      case _ => "*" #> ""
    }
  }

  def prev: CssSel = {
    val currentNeedId = S.param("id").openTheBox.toLong
    val prevNeed =
      Need.find(OrderBy(Need.id, Descending),
        By_<(Need.id, currentNeedId))

    prevNeed match {
      case Full(p) => {
        "a [href]" #> ("/read/" + p.id.get) &
          "a *" #> p.title.get
      }
      case _ => "*" #> ""
    }
  }

  def next: CssSel = {
    val currentNeedId = S.param("id").openTheBox.toLong
    val nextNeed =
      Need.find(OrderBy(Need.id, Ascending),
        By_>(Need.id, currentNeedId))

    nextNeed match {
      case Full(p) => {
        "a [href]" #> ("/read/" + p.id.get) &
          "a *" #> p.title.get
      }
      case _ => "*" #> ""
    }
  }

  //Count the number of posts that posted by current user
  def countByUser: CssSel = {
    val userId = User.currentUserId.openTheBox
    val count = 0 //Need.count(By(Need.author, userId.toLong))
    "span" #> count
  }

  //list posts posted by this user
  /*def listByUser: CssSel = {
    val userId = User.currentUserId.openTheBox
    val posts = Need.findAll(By( userId.toLong, userId.toLong))

    var odd = "even"
    "*" #> posts.map {
      post =>
        odd = YabeHelper.oddOrEven(odd)

        "p [class]" #> ("post " + odd) &
          "a [href]" #> ("/admin/posts/edit/" + post.id) &
          "a *" #> post.title
    }
  }    */

  def add: CssSel = {
    val post = Need.create

    def process() = {
      post.postedAt.set(new Date())
      //post.email.set(S.param("email").openTheBox)
      post.validate match {
        case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags").openTheBox)
          S.redirectTo("/success/" + post.id + "/" + YabeHelper.generateAdminHash(post.id))
        }
        case errors => S.error(errors)
      }
    }

    "#post-add" #> bindModel(post) _ &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }

  def edit: CssSel = {
    val id = S.param("id").openTheBox
    val post = Need.find(By(Need.id, id.toLong)).openTheBox

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
    val post = Need.find(By(Need.id, id.toLong)).openTheBox
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

  def getAdministration: CssSel = {
      val id = S.param("id").openTheBox
      val post = Need.find(By(Need.id, id.toLong)).openTheBox
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