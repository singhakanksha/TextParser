package io.cotiviti.parboiledStuff

import org.parboiled2._

import scala.util.{Failure, Success, Try}

case class Release(projectName:String= "abc",branch:String = "master",hash:String = "abc")

object Compose {
  val releaseMap = scala.collection.mutable.Map("p" -> "","b" -> "","h" -> "123abc456")
}

//release0--------------------------------------------------------------------------------------------------------------yy
class ReleaseDslParser(val input: ParserInput) extends Parser {

  protected def whiteSpaceChar = CharPredicate(" \n\r\t\f")

  protected def ws = rule {
    quiet(zeroOrMore(whiteSpaceChar))
  }

  // def Statement:Rule1[String] = rule { str("release") ~> ((releaseName: String) => {("")}) }// ~ str("help") ~> ((s:String,s1:String) => s.toString.concat(s1))}

  def releaseExtractor: Rule1[String] = rule {
    ReleaseString ~>
      ((x: String) => "Which project you wish to release, here is a list: \n" + Utils.repoMap.get("repositories").get.mkString(","))
  }

  def Release: Rule0 = rule(ws ~ "release" ~ ws)

  def ReleaseString: Rule1[String] = rule(capture(Release))

}

object ReleaseDslParser {
  def apply(input: String):String = {
    val inpLower = input.toLowerCase
    println(inpLower)
    val parser = new ReleaseDslParser(inpLower)
    val parserResult: Try[String] = parser.releaseExtractor.run()
    parserResult match {
      case Success(str) => str
      case Failure(e) => throw new RuntimeException("failed" + e.getCause)
    }

  }
}
//Project name--------------------------------------------------------------------------------------------------------------yy

class ProjectNameDslParser(val input: ParserInput) extends Parser {

  val reposAvailable = Utils.repoMap.get("repositories").get

  def nameExtractor: Rule1[String] = rule {
    projectName ~>
      ((repoName: String) => {
       val replyString =  if(reposAvailable.contains(repoName)){
          Compose.releaseMap.update("p",repoName)

          "valid repo name, please specify which branch to release from example: master"
        }
        else{ "invalid repo name, please try again"}
        replyString
      })
  }

  def ProjectNames = rule{ oneOrMore(ANY) }

  def projectName:Rule1[String] = rule(capture(ProjectNames))


}

object ProjectNameDslParser {
  def apply(input: String) = {
    val parser = new ProjectNameDslParser(input)
    val parserResult: Try[String] = parser.nameExtractor.run()
    parserResult match {
      case Success(str) => println(str)
      case Failure(e) => throw new RuntimeException("failed" + e.getCause)
    }
  }
}
//branch name--------------------------------------------------------------------------------------------------------------yy
object BranchNameDslParser{
  def apply(input: String) = {
    val parser = new BranchNameDslParser(input)
    val parserResult: Try[String] = parser.nameExtractor.run()
    parserResult match {
      case Success(str) => println(str)
      case Failure(e) => throw new RuntimeException("failed" + e.getCause)
    }
  }
}

class BranchNameDslParser(val input: ParserInput) extends Parser {
  import org.json4s.jackson.JsonMethods._
  import org.json4s.{DefaultFormats, Extraction}
  implicit lazy val serializerFormats = DefaultFormats
  def jsonify(): String ={
    val map = Compose.releaseMap
      val release =  Release(branch = map.getOrElse("b","branch"),projectName = map.getOrElse("p","project"),hash = "sbd")
    val jValue = Extraction.decompose(release)
    val jsonString = compact(jValue)
    jsonString
  }

  def nameExtractor: Rule1[String] = rule {
    BranchName ~>
      ((branchName: String) => {
        Compose.releaseMap.update("b",branchName)
          "valid branch name: Following request is under process:\n" + jsonify
      })
  }

  def BranchNames = rule{ str("master") | str("devel") | str("test") }

  def BranchName:Rule1[String] = rule(capture(BranchNames))
}







object Test1 extends App {

  val releaseCustomized = Release

  val str =  ReleaseDslParser("release")
//  ProjectNameDslParser("NexGen")
//  BranchNameDslParser("master")
  //  parsingResult match {
  //    case Success(e) => println(e)
  //    case Failure(e) => throw new RuntimeException("failed" + e.getCause)
  //  }
}
