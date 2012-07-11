package code.snippet

import xml.NodeSeq
import net.liftweb.http.{S, FileParamHolder, StatefulSnippet, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.sitemap.Loc.Snippet
import net.liftweb.common.{Full, Box, Empty}
import java.awt.Color
import code.session.SessionState

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher
 * Date: 05.07.12
 * Time: 17:10
 * To change this template use File | Settings | File Templates.
 */

class AddPost {

  def submitForm(in: NodeSeq): NodeSeq = {
    var postType: String = ""
    var postTitle = "Title..."
    var description = "Description..."
    var zipCode = "1234"

    val radioItemNames: Seq[String] = Seq("Need", "Offer")


    def processCreatePost {
      println("processCreatePost")

      // ToDo: process form values

      if (SessionState.isLoggedIn())
        S.redirectTo("/admin/post/created")
      else {
        //generate new Username
        SessionState.loginWithNewUserName()
        S.redirectTo("/admin/users/login")
      }

    }

    bind("e", in,
      "post_type" -> SHtml.radio(radioItemNames, Empty, (a:String) => postType = a).toForm,
      "post_title" -> SHtml.text(postTitle, postTitle = _),
      "description" -> SHtml.textarea(description, description = _,
        "cols" -> "80", "rows" -> "8"),
      "zip_code" -> SHtml.text(zipCode, zipCode = _),
      "submit_post" -> SHtml.submit("Create post", () => processCreatePost)
    )
  }



  var fileHolder: Box[FileParamHolder] = Empty

  def uploadPreview(in: NodeSeq): NodeSeq = {
    var test = "path to picture..."

    def processUploadPicture {
      println("processUploadPicture")
    }

    bind("e", in,
      "preview_picture" -> SHtml.fileUpload(fh => fileHolder = Full(fh)),
      "submit_picture" -> SHtml.submit("Upload picture", () => processUploadPicture)
    )
  }



}
