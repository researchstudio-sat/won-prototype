package code.snippet

import xml.{XML, NodeSeq}
import net.liftweb.util.Helpers._
import net.liftweb.http.{RedirectResponse, S, Templates, SHtml}
import net.liftweb.common.{Full, Empty}
import code.session.SessionState
import net.liftweb.sitemap.{Loc, **, Menu, MenuSingleton}
import net.liftweb.sitemap.Loc.{If, Test, Template}
import net.liftweb.sitemap.Menu.ParamMenuable
import code.service.UserService
import net.liftweb.util.{CssSel, CssSelector}

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 10.07.12
 * Time: 14:55
 * To change this template use File | Settings | File Templates.
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


