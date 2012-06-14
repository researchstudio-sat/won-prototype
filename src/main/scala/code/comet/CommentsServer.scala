package code.comet

import net.liftweb._
import http._
import actor._
import net.liftweb.mapper._
import code.model._

object  OffersServer extends LiftActor with ListenerManager {
  private var offers = List[Offer]()
  
  def createUpdate = offers
  
  override def lowPriority = {
    case postId:Long => {
      offers = Offer.findAll(By(Offer.post,postId))
      updateListeners()
    }
  }
}