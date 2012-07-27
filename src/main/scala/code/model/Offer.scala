package code.model


import net.liftweb.mapper._
import net.liftweb.util._
import scala.xml.{NodeSeq, Text, Node}
import code.lib.YabeHelper
import xml.{Node, Text}
import net.liftweb.common.{Box, Full}

class Offer extends LongKeyedMapper[Offer] with IdPK {
  def getSingleton = Offer

  object author extends MappedString(this, 140) {
    override def asHtml = {
      this.get match {
        case "" => Text("guest")
        case _ => Text(this.get)
      }
    }
  }

  object content extends MappedTextarea(this, 1000) {
    override def validations = {
      def notNull(txt: String) = {
        if (txt == "")
          List(FieldError(this, "Please input description$."))
        else
          List[FieldError]()
      }

      notNull _ :: Nil
    }

    def short: Node = {
      this.get.length match {
        case l if l > 50 => Text(this.get.substring(50) + "...")
        case _ => Text(this.get)
      }
    }
  }

  object email extends MappedEmail(this,500) {

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
        case _ => {
          Full(this.set(null))
        }
      }
    }
  }

  object post extends MappedLongForeignKey(this, Need) {

    override def validSelectValues = {
      val posts = Need.findAll().map((x:Need) => (x.id.get, x.title.get))
      val list = (0.toLong, "(Please select a post)") :: posts
      Full(list)
    }

    override def validations = {
      def validateNeed(id: Long) = {
        val posts = Need.findAll(By(Need.id, id))
        posts match {
          case Nil => List(FieldError(this, "Please add offers to valid posts."))
          case _ => List[FieldError]()
        }
      }

      validateNeed _ :: Nil
    }

    override def asHtml = {
      val post = Need.find(By(Need.id, this.get))
      post match {
        case Full(p) => Text(p.title.get)
        case _ => Text("")
      }
    }
  }

}

object Offer extends Offer with LongKeyedMetaMapper[Offer] with CRUDify[Long, Offer]
