package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common.{Box, Empty, Full}
import code.lib.YabeHelper
import xml.{Unparsed, Text, Node}
import net.liftweb.textile.TextileParser


class Need extends LongKeyedMapper[Need] with IdPK {
  def getSingleton = Need

  /*
object author extends MappedLongForeignKey(this, User) {
override def validSelectValues = {
  val users = User.findAll().map( (x:User) => (x.id.get, x.email.get) )
  val list = (0.toLong, "(Please select a user)") :: users
  Full(list)
}

override def validations = {
  def needAuthor(author: Long) = {
    if (author == 0) List(FieldError(this, "Please select a user."))
    else List[FieldError]()
  }

  needAuthor _ :: Nil
}

def getAuthor = {
  User.find(By(User.id, author.get)).openTheBox
}

def first:Node = {
  Text(getAuthor.firstName.get)
}

def last:Node = {
  Text(getAuthor.lastName.get)
}
}    */

  object title extends MappedString(this, 140) {
    override def validations = {
      valMinLen(1, "Please input title.") _ :: Nil
    }
  }

  object email extends MappedEmail(this, 500) {

  }

  object content extends MappedTextarea(this, 1000) {
    override def validations = {
      def notNull(txt: String) = {
        if (txt == "")
          List(FieldError(this, "Please input content."))
        else
          List[FieldError]()
      }

      notNull _ :: Nil
    }

    override def asHtml: Node = {
      Unparsed(TextileParser.toHtml(this.get.toString).toString)
    }
  }

  object postedAt extends MappedDateTime(this) {
    override def validations = {
      def isDate(txt: java.util.Date) = {
        if (txt == null)
          List(FieldError(this, "Please input a validate date."))
        else
          List[FieldError]()
      }

      isDate _ :: Nil
    }

    override def format(d: java.util.Date): String = YabeHelper.fmtDateStr(d)

    override def parse(s: String): Box[java.util.Date] = {
      val df = new java.text.SimpleDateFormat("yyyy-MM-dd")
      try {
        val date = df.parse(s)
        Full(date)
      } catch {
        case _ => Full(this.set(null))
      }
    }
  }

  object tags extends HasManyThrough(this, Need, NeedTag, NeedTag.tag, NeedTag.post) {
    def setMultiple(post: Need, tags: String) {
      //delete old tag relation
      NeedTag.findAll(By(NeedTag.post, post.id)).foreach(_.delete_!)

      //set new tag relation
      Tag.createAndGetTags(splitTags(tags)).distinct.foreach(NeedTag.join(post, _))
    }

    //Format tags as "tag1 tag2 tag3.."
    def concat: String = {
      val postTags = NeedTag
        .findAll(By(NeedTag.post, Need.this.id))

      postTags match {
        case Nil => ""
        case _ => postTags
          .map((pt: NeedTag) => Tag.getName(pt.tag.get))
          .reduceLeft(_ + " " + _)
      }
    }

    private def splitTags(tags: String) = {
      tags.split(" ").toList
    }
  }

  //delete its offers first
  override def delete_!(): Boolean = {
    val offers = Offer.findAll(By(Offer.post, this.id.get))
    offers.foreach(_.delete_!)

    super.delete_!
  }

  def countOffers: Long = {
    Offer.count(By(Offer.post, this.id))
  }

  def latestOfferAuthor: String = {
    if (countOffers <= 0)
      ""
    else {
      val latest = Offer.find(By(Offer.post, this.id),
        OrderBy(Offer.postedAt, Descending)).openTheBox

      latest.author match {
        case author if (author.length > 0) => ", lastest by " + latest.author
        case _ => ", lastest by guest"
      }
    }
  }

  def showTagMetaStr: String = {
    val postTags = NeedTag.findAll(By(NeedTag.post, this.id))
    val tagNames = postTags.map {
      (pt: NeedTag) =>
        val tag = Tag.find(By(Tag.id, pt.tag)).openTheBox
        "<a href='/posts/" + tag.name.get + "'>" + tag.name.get + "</a>"
    }
    if (tagNames.length > 0)
      " - tagged: " + tagNames.reduceLeft(_ + ", " + _)
    else
      " - no tags"
  }
}

object Need extends Need with LongKeyedMetaMapper[Need] with CRUDify[Long, Need] {
  def getNeedsByTag(tag: String) = {
    val tagId = Tag.find(By(Tag.name, tag)) match {
      case Full(t) => t.id.get
      case _ => -1
    }

    val posts =
      Need.findAll(In(Need.id, NeedTag.post, By(NeedTag.tag, tagId)),
        OrderBy(Need.id, Descending))

    posts
  }
}
