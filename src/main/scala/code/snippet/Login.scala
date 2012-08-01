package code.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.{RedirectResponse, SHtml}
import net.liftweb.common.Full
import net.liftweb.sitemap.{**, Menu}
import net.liftweb.sitemap.Loc.If
import code.service.UserService
import net.liftweb.util.CssSel

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 10.07.12
 * Time: 14:55
 * To change this template use File | Settings | File Templates.
 */


/**
 * Provides snippets for User login and logout.
 */
class Login {
  def anonLogin(in: NodeSeq): NodeSeq = {

    val adminURL = "/login/key/" +UserService.getAdminKeyForLoggedInUser.openOr("N/A");

    bind("login", in,
      "admin_url" -> SHtml.link(adminURL, () => {}, <span>
        {adminURL}
      </span>)
    )
  }

  def getTopBarUsername: CssSel = "*" #> {"UserId: " + UserService.isUserLoggedIn.openOr("N/A").toString}

  def logout(in: NodeSeq) = {
    UserService.logout
    in
  }
}

/**
 * This object provides the methods to create the Admin Key URL menu and methods to validate the
 * admin key if the according URL is requested.
 */
object LoginMenu {

  private var adminKey: String = ""

  case class AdminKey(value: String)

  def parse(b: String) = {
    adminKey = b;
    Full(AdminKey(b))
  }

  def encode(c: AdminKey) = c.value

  def checkAdminKey: RedirectResponse = {
    val userId = UserService.validateAdminLink(adminKey)
    if (userId.isDefined) {
      UserService.setUserIdSessionState(userId.openTheBox)
      RedirectResponse("/login/success")
    } else {
      UserService.logout
      RedirectResponse("/login/fail")
    }
  }

  def getMenu = {
    Menu.param[AdminKey]("AdminKeyLogin", "Login With Admin Key",
      parse _,
      encode _) / "login" / "key" / ** >> If(() => false, checkAdminKey _)
  }
}


