package code.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.common.Empty
import code.session.SessionState

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 10.07.12
 * Time: 14:55
 * To change this template use File | Settings | File Templates.
 */

class Login {

  def anonLogin(in: NodeSeq): NodeSeq = {

    val adminURL =  "/admin/user/" + SessionState.loggedInUserName.get

    bind("login", in,
      "admin_url" -> SHtml.link(adminURL, () => {}, in)
    )
  }

}
