package bootstrap.liftweb

import net.liftweb._
import common.{Empty, Full, EmptyBox, Box}
import util._
import http._
import sitemap._
import Loc._
import mapper._
import code.model._
import code.lib._
import xml.{Text, NodeSeq}
import code.snippet.LoginMenu


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)


    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Post, Tag, PostTag, PostImage)

    // where to search snippet
    LiftRules.addToPackages("code")
    LiftRules.addToPackages("code/snippet")


    def menus =

    // login menu
      LoginMenu.getMenu :: List(

        // top menu
        Menu.i("Home") / "index",
        Menu.i("Activities") / "activities",

        // bottom menu
        Menu.i("Imprint") / "imprint",
        Menu.i("Contact") / "contact",
        Menu.i("Privacy") / "privacy",
        Menu.i("Terms") / "terms",
        Menu.i("Features") / "features",
        Menu.i("About") / "about",

        // administration
        Menu.i("User List") / "admin" / "users" / "index",


        // posts
        Menu.i("Create Post") / "admin" / "post" / "add",
        Menu.i("Post Created") / "admin" / "post" / "created",
        Menu.i("Relation") / "relation",
        Menu.i("Post List") / "admin" / "post" / "index",

        // pages for the user management and login
        Menu.i("User Login") / "login" / "index",
        Menu.i("User Login Success") / "login" / "success",
        Menu.i("User Login Fail") / "login" / "fail",
        Menu.i("User Logout") / "logout"

      )

    // Build SiteMap
    def sitemap = SiteMap(
      menus: _*
    )

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just offer this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    //Rewrite
    /*
    LiftRules.statelessRewrite.append {


      //successfully created Need1
      case RewriteRequest(ParsePath("success" :: id :: token :: Nil, _, _, _), _, _) =>
        RewriteResponse("success" :: Nil, Map("id" -> id, "token" -> token))

    }
    */


    //Create Demo Users
    initUsers()

    // Use jQuery 1.4
    //LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    //LiftRules.ajaxStart =
    //  Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    //LiftRules.ajaxEnd =
    //  Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }


  //Init user data, one super user and one normal user.
  def initUsers() {
    if (isDemoSuperUserExist == false) {
      val superUser = User.create
        .firstName("Super")
        .lastName("Demo")
        .email("super@demo.com")
        .password("demouser")
        .superUser(true)
        .isDemo(true)
        .validated(true)

      superUser.save
    }

    if (isDemoNormalUserExist == false) {
      val normalUser = User.create
        .firstName("Normal")
        .lastName("Demo")
        .email("normal@demo.com")
        .password("demouser")
        .superUser(false)
        .isDemo(true)
        .validated(true)

      normalUser.save
    }
  }

  def isDemoSuperUserExist() = {
    User.count(By(User.isDemo, true), By(User.superUser, true)) match {
      case x if x > 0 => true
      case _ => false
    }
  }

  def isDemoNormalUserExist() = {
    User.count(By(User.isDemo, true), NotBy(User.superUser, true)) match {
      case x if x > 0 => true
      case _ => false
    }
  }
}
