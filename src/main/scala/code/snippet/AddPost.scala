package code.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.common.{Box, Full, Empty}
import code.session.SessionState
import code.service.UserService
import code.model.Need
import code.model.PostImage
import net.liftweb.http.{SHtml, S, FileParamHolder}
import java.io.FileOutputStream
import java.io.File
import code.wonConfiguration

/**
 * Created with IntelliJ IDEA.
 * User: fsalcher, SK
 * Date: 05.07.12
 * Time: 17:10
 * To change this template use File | Settings | File Templates.
 */

class AddPost {

  def submitForm(in: NodeSeq): NodeSeq = {

    //    var pType: String = ""
    var postIntention: String = ""
    var postTitle = "Title..."
    var postDescription = "Description..."

    //    val typeRadioItemNames: Seq[String] = Seq("Need", "Offer")
    val intentionRadioItemNames: Seq[String] = Seq("Give", "Take", "Do")

    // Add a variable to hold the FileParamHolder on submission
    var fileHolder: Box[FileParamHolder] = Empty

    def processCreatePost {

      //TODO: Get the user id: The below commented lines
      //      if (SessionState.isLoggedIn())
      //        S.redirectTo("/admin/post/created")
      //      else {
      //        //generate new Username
      //        SessionState.loginWithNewUserName()
      //        S.redirectTo("/admin/users/login")
      //      }

      if (UserService.isUserLoggedIn.isDefined)
        S.redirectTo("/admin/post/created")
      else {
        //generate new Username
        val userId = UserService.createNewAdminLink
        UserService.setUserIdSessionState(userId)
        S.redirectTo("/login")

      //TODO: Add the user id to the Need
      Need.create.title(postTitle).description(postDescription).intention(postIntention).postedAt(timeNow).save()

      //Saving the image
      if (!fileHolder.isEmpty) {
        if (fileHolder.map(_.mimeType).openTheBox.startsWith("image/") && fileHolder.map(_.file).openTheBox.length > 0) {
          //TODO: If possible just replace the findAll with find with the last id, to improve the performance!
          PostImage.create.needID(Need.findAll()(Need.findAll().size - 1).id.toString()).save() //get the id of the last need
          val filePath = wonConfiguration.imagesPath
          val imageTypeArray = fileHolder.map(_.fileName).openTheBox.split('.')
          val oFile = new File(filePath, PostImage.findAll()(PostImage.findAll().size - 1).id.toString() + "." + imageTypeArray(imageTypeArray.length - 1)) //get the id of the last image
          val output = new FileOutputStream(oFile)
          output.write(fileHolder.map(_.file).openTheBox)
          output.close()
        }
      }

      //You can use the following lines to make the mapper empty, if needed for debug
      /*      println("DEBUG: size of Need Mapper")
      println(Need.findAll().size)
      println(Need.findAll())
      for (i <- 0 until (Need.findAll().size - 1)) {
        Need.delete_!(Need.findAll()(i))
      }
      Need.delete_!(Need.findAll()(0))
      println("DEBUG: size of Need Mapper")
      println(Need.findAll().size)
      println(Need.findAll())
      println(PostImage.findAll())
      println("processCreastePost")*/

    }

    bind("e", in,
      //      "post_type" -> SHtml.radio(typeRadioItemNames, Empty, (a: String) => pType = a).toForm,
      "post_intention" -> SHtml.radio(intentionRadioItemNames, Empty, (a: String) => postIntention = a).toForm,
      "post_title" -> SHtml.text(postTitle, postTitle = _),
      "description" -> SHtml.textarea(postDescription, postDescription = _,
        "cols" -> "80", "rows" -> "8"),
      "receipt" -> SHtml.fileUpload((f: FileParamHolder) => fileHolder = Full(f)),
      "submit_post" -> SHtml.submit("Create post", () => processCreatePost)
    )
  }


}
