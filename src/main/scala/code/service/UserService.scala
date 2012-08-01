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

// The UserService object provides methods for the Login/Logout mechanism and the admin key administration.

object UserService {

  private object loggedInUser extends SessionVar[Box[Long]](Empty)

  /**
   * Checks if a given admin link is valid an returns the associated userId
   * @param link the administration link
   * @return a Box with the userId
   */
  def validateAdminLink(link: String): Box[Long] = {

    val filteredPosts: List[User] = User.findAll(Like(User.adminLink, link))
    filteredPosts.length match {
      case 1 => Full(filteredPosts(0).id)
      case 0 => Empty
      case _ => Failure("Multiple users found to admin link " + link + "!")
    }
  }

  /**
   * Creates a new admin link and a new user to the admin link
   * @return the userId of the newly created user
   */
  def createNewAdminLink: Long = {

    val key = uniqueRandomKey(6)
    val user = User.create
    user.adminLink.apply(key)
    user.save

    return user.id.get
  }

  /**
   * returns the userId of the logged in user or Empty if no user is logged in
   * @return Box with the userId
   */
  def isUserLoggedIn: Box[Long] = {
    loggedInUser.is
  }

  /**
   * Saves the given userId in a session variable as logged in.
   * @param userId userId of the logged in user
   */
  def setUserIdSessionState(userId: Long) = {
    loggedInUser.set(Box(userId))
  }

  /**
   * Makes a logout of the current user.
   */
  def logout = {
    loggedInUser.set(Empty)
  }

  /**
   * Generates a random key of a given length containing the characters a-z, A-Z and 0-9.
   * The method ensures that the generated key is not already used as admin url for a user.
   * @param length length of the key to generate
   * @return the generated key
   */
  def uniqueRandomKey(length: Int): String = {

    // this method checks if the given key is already used as admin key url
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

  /**
   * If a user is logged in this method returns the admin key for this user.
   * @return Box with the admin key or Empty if no user is logged in.
   */
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