package code.comet

import scala.xml.Unparsed
import net.liftweb._
import http._
import util._
import Helpers._
import code.model._
import code.lib._

class OffersList extends CometActor with CometListener {
  private var offers = List[Offer]()
  
  def registerWith = OffersServer
  
  override def lowPriority = {
    case c:List[Offer] => offers = c; reRender
  }
  
  def render = {
    ".offer-count *" #> (offers.length + " offers") &
    ".offer" #> offers.map {
      c => 
        ".offer-author *" #> (c.author.get match {
          case "" => "guest"
          case _ => c.author.get
        })  & 
        ".offer-date *" #> YabeHelper.fmtDateStr(c.postedAt.get) &
        ".offer-description$-span" #> Unparsed(c.content.get.replaceAll("\n","<br />"))
    }
  }
}