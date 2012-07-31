package code.service

import net.liftweb.http.SessionVar
import net.liftweb.common.{Failure, Full, Empty, Box}
import net.liftweb.mapper.{By, Like}
import code.model.User
import util.Random

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 26.07.12
 * Time: 09:24
 * To change this template use File | Settings | File Templates.
 */

object UserService {

  private object loggedInUser extends SessionVar[Box[Long]](Empty)

  /* returns the user id of the admin link or Empty */
  def validateAdminLink(link: String): Box[Long] = {
    // ToDo: implement
    //    link match {
    //      case "aa" => Full(11)
    //      case "bb" => Full(22)
    //      case "cc" => Full(33)
    //      case _ => Empty
    //    }

    val filteredPosts: List[User] = User.findAll(Like(User.adminLink, link))
    filteredPosts.length match {
      case 1 => Full(filteredPosts(0).id)
      case 0 => Empty
      case _ => Failure("Multiple users found to admin link " + link + "!")
    }
  }

  /* creates a new admin link and a new user to the admin link; returns the userId of the
   * newly created user
   */
  def createNewAdminLink: Long = {
    // ToDo: implement
    // generate new link
    //    val newLink = "aa"
    //    return newLink

    val key = uniqueRandomKey(6)
    val user = User.create
    user.adminLink.apply(key)
    user.save

    return user.id.get
  }

  /* returns the userId of the logged in user or Empty */
  def isUserLoggedIn: Box[Long] = {
    // ToDo: implement
    loggedInUser.is
  }

  def setUserIdSessionState(userId: Long) = {
    loggedInUser.set(Box(userId))
  }

  def logout = {
    loggedInUser.set(Empty)
  }


  def uniqueRandomKey(length: Int): String = {
    def isUnique(s: String): Boolean = {
      val filteredPosts: List[User] = User.findAll(Like(User.adminLink, s))
      if (filteredPosts.length > 0)
        false
      else
        true
    }

    val chars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).mkString("")
    val newKey = (1 to length).map(
      x => {
        val index = Random.nextInt(chars.length)
        chars(index)
      }
    ).mkString("")
    if (isUnique(newKey))
      newKey
    else
      uniqueRandomKey(length)
  }

  def getAdminKeyForLoggedInUser: Box[String] = {
    if (isUserLoggedIn.isDefined) {
      val users: List[User] = User.findAll(By(User.id, isUserLoggedIn.openTheBox))
      if (users.length == 0 || users.length > 1)
        Empty
      else
        new Full[String](users(0).adminLink)
    }
    else {
      return Empty
    }
  }
}