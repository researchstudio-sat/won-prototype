package code.model

import net.liftweb.mapper._

/**
 * Author: SK
 * Date: 27.07.12
 * Time: 16:57
 */

class PostImage extends LongKeyedMapper[PostImage] with IdPK {
  def getSingleton = PostImage

  object postID extends MappedString(this, 999) {
    override def validations = {
      valMinLen(1, "Please input the postID.") _ :: Nil
    }
  }
}

object PostImage extends PostImage with LongKeyedMetaMapper[PostImage] with CRUDify[Long, PostImage] {
}