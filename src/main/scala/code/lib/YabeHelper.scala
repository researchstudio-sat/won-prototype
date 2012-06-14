package code.lib


import net.liftweb.http._
import java.util.Random
import java.awt.image.BufferedImage

import java.awt.Font
import java.awt.Color
import java.awt.GradientPaint
import java.awt.RenderingHints
import java.io._
import javax.imageio.ImageIO
import net.liftweb.common.{Full, Box}
import net.liftweb.util.SecurityHelpers._
import net.liftweb.util.Props
import org.apache.commons.codec.binary.Base64


object YabeHelper {

  /**
   * This fucntion generates the security token for Entries
   * @param id
   * @return the security token for the given id
   */
  def generateHash(id: Long): String = {
    hashHex(Props.get("security_salt").openTheBox.toString + id)
  }

  /**
   * This fucntion generates the security token for Admin Sites
   * @param id
   * @return the security token for the given id
   */
  def generateAdminHash(id: Long): String = {
    hashHex(Props.get("security_salt").openTheBox.toString + id + "!admin")
  }

  /**
   * Control list style from posts, users, etc..
   */
  def oddOrEven(current: String) = {
    current match {
      case "odd" => "even"
      case _ => "odd"
    }
  }

  def fmtDateStr(date: java.util.Date) = {
    date match {
      case null => ""
      case _ => val format = new java.text.SimpleDateFormat("yyyy-MM-dd"); format.format(date)
    }
  }

}