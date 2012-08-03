package code.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.common.{Box, Full, Empty}
import code.service.UserService
import code.model.{Post, PostImage}
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


/**
 * In this class the form elements of the add post site are created. Also the submit request is handled
 * in this class. This includes the check for an logged in user, writing the post into the DB and storing
 * the associated picture in the file system.
 */
class AddPost {


  def submitForm(in: NodeSeq): NodeSeq = {

    // The associated variables to the form elements with the initial values.
    var postIntention: String = ""
    var postTitle = "Title..."
    var postDescription = "Description..."

    val intentionRadioItemNames: Seq[String] = Seq("Give", "Take", "Do")

    // Here the uploaded file an additional information is stored after submitting the form.
    var fileHolder: Box[FileParamHolder] = Empty

    def processCreatePost {

      // Check if a user is logged in or create a new user and redirect to the according sites.
      var userId: Long = 0
      var redirectLink = ""
      if (UserService.isUserLoggedIn.isDefined) {
        redirectLink = "/admin/post/created"
        userId = UserService.isUserLoggedIn.openTheBox

      } else {
        // Create a new user.
        userId = UserService.createNewAdminLink
        UserService.setUserIdSessionState(userId)
        redirectLink = "/login"
      }

      // Create and save the new post.
      val post: Post = Post.create
      post.title(postTitle).description(postDescription).intention(postIntention).postedAt(timeNow).userID(userId).save()
      post.primaryKeyField

      // Save the image into the file system
      if (!fileHolder.isEmpty) {
        if (fileHolder.map(_.mimeType).openTheBox.startsWith("image/") && fileHolder.map(_.file).openTheBox.length > 0) {
          PostImage.create.postID(post.primaryKeyField.toString).save() //get the id of the last need
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

      S.redirectTo(redirectLink)
    }

    bind("e", in,
      "post_intention" -> SHtml.radio(intentionRadioItemNames, Empty, (a: String) => postIntention = a).toForm,
      "post_title" -> SHtml.text(postTitle, postTitle = _),
      "description" -> SHtml.textarea(postDescription, postDescription = _,
        "cols" -> "80", "rows" -> "8"),
      "receipt" -> SHtml.fileUpload((f: FileParamHolder) => fileHolder = Full(f)),
      "submit_post" -> SHtml.submit("Create post", () => processCreatePost)
    )
  }
}
