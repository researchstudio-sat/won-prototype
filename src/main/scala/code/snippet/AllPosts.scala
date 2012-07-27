package code.snippet

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._
import code.model._
import code.lib._
import net.liftweb.mapper.By._
import code.lib.ModelBinder._

class AllNeeds {

  private object searchStr extends RequestVar("")

  def list: CssSel = {
    val posts = getNeeds()
    var odd = "even"
    "tr" #> posts.map {
      p =>
        odd = YabeHelper.oddOrEven(odd);
        "tr [class]" #> odd &
          "a [href]" #> ("/admin/all_posts/edit/" + p.id) &
          "a *" #> p.title
    }
  }

  def delete: CssSel = {
    val id = S.param("id").openTheBox
    val post = Need.find(By(Need.id, id.toLong)).openTheBox

    def process() = {
      post.delete_!
      S.redirectTo("/admin/all_posts/index")
    }

    "type=submit" #> SHtml.onSubmitUnit(process)
  }


  def search: CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }

  def count = {
    "span" #> countNeeds
  }

  def sort = {
    val search = searchStr.is

    if (getNeedsOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
        "a" #> SHtml.link("/admin/all_posts/index?order=ASC",
          () => searchStr(search),
          <span>Needs</span>,
          "class" -> "crudSortedDesc")
    else
      "a [class]" #> "crudSortedAsc" &
        "a" #> SHtml.link("/admin/all_posts/index?order=DESC",
          () => searchStr(search),
          <span>Needs</span>,
          "class" -> "crudSortedAsc")
  }

  private def countNeeds() = {
    if (validSearch()) {
      Need.count(BySql(" title like '%" + searchStr.is + "%' or description$ like '%" + searchStr.is + "%'",
        IHaveValidatedThisSQL("charliechen", "2011-07-11")))
    } else
      Need.count()
  }

  private def getNeeds() = {
    val posts = validSearch() match {
      case x if x == true => Need.findAll(
        BySql(" title like '%" + searchStr.is + "%' or description$ like '%" + searchStr.is + "%'",
          IHaveValidatedThisSQL("charliechen", "2011-07-11")),
        OrderBy(Need.title, Ascending))

      case _ => Need.findAll(OrderBy(Need.title, Ascending))
    }

    getNeedsOrder match {
      case "DESC" => posts.reverse
      case "ASC" => posts
    }
  }

  private def validSearch() = searchStr.is != ""


  private def getNeedsOrder = {
    S.param("order") match {
      case Full(p) if p == "DESC" => "DESC"
      case _ => "ASC"
    }
  }
}

class AllNeedsAdd extends StatefulSnippet {

  val post = Need.create

  def dispatch = {
    case "render" => render
  }

  def render = {
    def process() = {
      post.validate match {
       case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags_name_list").openTheBox)
          S.redirectTo("/admin/all_posts/index")
        }
        case errors => S.error(errors)
      }
    }


    "#post-display" #> bindModel(post) _ &
      renderTags(NeedTag.findAll(By(NeedTag.post, post.id)).map(_.tag.get)) &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }
}

class AllNeedsEdit extends StatefulSnippet {
  private val id = S.param("id").openTheBox
  private val post = Need.find(By(Need.id, id.toLong)).openTheBox

  def dispatch = {
    case "render" => render
  }

  def render: CssSel = {

    def process() = {
      post.validate match {
        case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags_name_list").openTheBox)
          S.redirectTo("/admin/all_posts/index")
        }
        case errors => S.error(errors)
      }
    }

    "#post-display" #> bindModel(post) _ &
      renderTags(NeedTag.findAll(By(NeedTag.post, post.id)).map(_.tag.get)) &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }
}

object renderTags {
  def apply(selectedTags:List[Long]): CssSel = {
    val tags = Tag.findAll
    var index = 0
    ".tags-list-item" #> tags.map {
      t =>
        index = index + 1
        val selectedName = selectedTags.contains(t.id.get) match {
          case true => t.name.get
          case false => ""
        }
        val selected = selectedName match {
          case "" => ""
          case _ => "selected"
        }
        ".tag [id]" #> index &
        ".tag *" #> t.name.get &
        ".tag [class]" #> ("tag "+selected) &
        "type=hidden [id]" #> ("h"+index) &
        "type=hidden [value]" #> selectedName
    }
  }
}