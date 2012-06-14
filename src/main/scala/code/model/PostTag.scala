package code.model

import net.liftweb.mapper._

class NeedTag extends LongKeyedMapper[NeedTag] with IdPK {
  def getSingleton = NeedTag
  object post extends MappedLongForeignKey(this, Need)
  object tag extends MappedLongForeignKey(this,Tag)
}

object NeedTag extends NeedTag with LongKeyedMetaMapper[NeedTag] {
    def join(p:Need, t:Tag) = this.create.tag(t).post(p).save
}