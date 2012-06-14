package code.snippet

import scala.xml.Unparsed
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import js._
import JsCmds._
import JE._
import java.util.Date
import Helpers._
import code.model._
import code.lib._
import ModelBinder._
import code.comet._

class Offers {

  object postId extends RequestVar(S.param("id").openTheBox.toLong)

  def add = {
    var offer = Offer.create
    //User curry function to keep postId, otherwise, postId will be lost in ajax request
    def process(id: Long)(): JsCmd = {
      offer.postedAt.set(new java.util.Date)
      offer.post.set(id)


      offer.validate match {
        case Nil => {
          offer.save
          //prepare for another offer
          offer = Offer.create

          OffersServer ! id

          JE.Call("clearError") & JE.Call("clearForm")
        }
        case errors => S.error(errors); JE.Call("clearError") & JE.Call("showError", Str(errors.head.msg.toString))
      }

    }

    "name=author" #> SHtml.text(offer.author.get, offer.author.set(_)) &
      "name=email" #> SHtml.text(offer.email.get, offer.email.set(_)) &
      "name=content" #> (SHtml.textarea(offer.content.get, offer.content.set(_), "id" -> "content") ++
        SHtml.hidden(process(postId.is)))
  }

  def initComet = {
    OffersServer ! postId.is
    "*" #> ""
  }

  /******************************************************************************************
   * For admin panel..
   *****************************************************************************************/
  private object searchStr extends RequestVar("")

  def list: CssSel = {
    val offers = getOffers()
    var odd = "even"

    "tr" #> offers.map {
      c =>
        odd = YabeHelper.oddOrEven(odd)
        ".offer_item" #> bindModel(c, {
          "tr [class]" #> odd
        }) _

    }
  }

  def delete = {
    val id = S.param("id").openTheBox
    val offer = Offer.find(By(Offer.id, id.toLong)).openTheBox
    def process() = {
      offer.delete_!
      S.redirectTo("/admin/offers/index")
    }
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def sort = {
    val search = searchStr.is

    if (getOffersOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
        "a" #> SHtml.link("/admin/offers/index?order=ASC",
          () => searchStr(search),
          <span>Contents</span>,
          "class" -> "crudSortedDesc")
    else
      "a [class]" #> "crudSortedAsc" &
        "a" #> SHtml.link("/admin/offers/index?order=DESC",
          () => searchStr(search),
          <span>Content</span>,
          "class" -> "crudSortedAsc")
  }

  def search: CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }

  def count = {
    "span" #> countOffers
  }


  private def countOffers() = {
    if (validSearch()) {
      Offer.count(BySql(" content like '%" + searchStr.is + "%' ",
        IHaveValidatedThisSQL("charliechen", "2011-07-21")))
    } else
      Offer.count()
  }

  private def getOffers() = {
    val offers = validSearch() match {
      case x if x == true => Offer.findAll(
        BySql(" content like '%" + searchStr.is + "%'",
          IHaveValidatedThisSQL("charliechen", "2011-07-21")),
        OrderBy(Offer.id, Ascending))

      case _ => Offer.findAll(OrderBy(Offer.content, Ascending))
    }

    getOffersOrder match {
      case "DESC" => offers.reverse
      case "ASC" => offers
    }
  }

  private def validSearch() = searchStr.is != ""

  private def getOffersOrder = {
    S.param("order") match {
      case Full(p) if p == "DESC" => "DESC"
      case _ => "ASC"
    }
  }
}

class OffersEdit extends StatefulSnippet {
  private val id = S.param("id").openTheBox
  private val offer = Offer.find(By(Offer.id, id.toLong)).openTheBox

  def dispatch = {
    case "render" => render
  }

  def render = {

    def process() = {
      offer.validate match {
        case Nil => {
          offer.save
          S.redirectTo("/admin/offers/index")
        }
        case errors => S.error(errors)
      }
    }

    "*" #>
      bindModel(offer, {
        "type=submit" #> SHtml.onSubmitUnit(() => process)
      }) _

  }
}