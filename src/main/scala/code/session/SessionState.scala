package code.session

import net.liftweb.common.{Box, Empty}
import net.liftweb.http.SessionVar

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 10.07.12
 * Time: 15:00
 * To change this template use File | Settings | File Templates.
 */

object SessionState {

  object loggedInUserName extends SessionVar[Box[String]](Empty)

  def isLoggedIn(): Boolean = {
    return (loggedInUserName.is != Empty)
  }

  def loginWithNewUserName(): String = {
    //ToDo: implement
    val userName = "newUserId"
    loggedInUserName.set(Box(userName))
    return (userName)
  }
}

