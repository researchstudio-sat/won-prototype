package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common.{Empty, Box, Full}
import code.lib.YabeHelper
import xml.{Unparsed, Text, Node}
import net.liftweb.textile.TextileParser
import net.liftweb.http.{RequestVar, FileParamHolder}


class Post extends LongKeyedMapper[Post] with IdPK {
  def getSingleton = Post

  object userID extends MappedLong(this) {
  }

  object intention extends MappedString(this, 140) {
    override def validations = {
      valMinLen(1, "Please input intention.") _ :: Nil
    }
  }

  object title extends MappedString(this, 140) {
    override def validations = {
      valMinLen(1, "Please input title.") _ :: Nil
    }
  }

  object description extends MappedTextarea(this, 1000) {
    override def validations = {
      def notNull(txt: String) = {
        if (txt == "")
          List(FieldError(this, "Please input description$."))
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

  object tags extends HasManyThrough(this, Post, PostTag, PostTag.tag, PostTag.post) {
    def setMultiple(post: Post, tags: String) {
      //delete old tag relation
      PostTag.findAll(By(PostTag.post, post.id)).foreach(_.delete_!)

      //set new tag relation
      Tag.createAndGetTags(splitTags(tags)).distinct.foreach(PostTag.join(post, _))
    }

    //Format tags as "tag1 tag2 tag3.."
    def concat: String = {
      val postTags = PostTag
        .findAll(By(PostTag.post, Post.this.id))

      postTags match {
        case Nil => ""
        case _ => postTags
          .map((pt: PostTag) => Tag.getName(pt.tag.get))
          .reduceLeft(_ + " " + _)
      }
    }

    private def splitTags(tags: String) = {
      tags.split(" ").toList
    }
  }

  def showTagMetaStr: String = {
    val postTags = PostTag.findAll(By(PostTag.post, this.id))
    val tagNames = postTags.map {
      (pt: PostTag) =>
        val tag = Tag.find(By(Tag.id, pt.tag)).openTheBox
        "<a href='/posts/" + tag.name.get + "'>" + tag.name.get + "</a>"
    }
    if (tagNames.length > 0)
      " - tagged: " + tagNames.reduceLeft(_ + ", " + _)
    else
      " - no tags"
  }
}

object Post extends Post with LongKeyedMetaMapper[Post] with CRUDify[Long, Post] {
  def getPostsByTag(tag: String) = {
    val tagId = Tag.find(By(Tag.name, tag)) match {
      case Full(t) => t.id.get
      case _ => -1
    }

    val posts =
      Post.findAll(In(Post.id, PostTag.post, By(PostTag.tag, tagId)),
        OrderBy(Post.id, Descending))

    posts
  }
}
