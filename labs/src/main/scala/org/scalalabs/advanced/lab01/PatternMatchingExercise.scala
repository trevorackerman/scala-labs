package org.scalalabs.advanced.lab01

import scala.xml._
import collection.mutable.{ ListBuffer => MList }
import scala.None

/**
 * Scala's pattern matching features are very powerful. Learn
 * how to unleash that power by solving the exercises
 * contained in this class. The exercises cover the following topics:
 * - argument extractors
 * - regexp matching
 * - xml matching
 */
object PatternMatchingExercise {

  /**
   * ***********************************************************************
   * CUSTOM ARGUMENT EXTRACTORS
   * ***********************************************************************
   */

  /**
   * Use the FileName object to write a simple argument extractor,
   * which extracts filename and extension. Basically,
   * implement an apply and unapply method.
   * Example:
   * MyNotes.txt
   * -> MyNotes txt
   */
  object FileName {
    var fileName = ""
    var extension = ""

    def unapply(name: String): Option[(String, String)] = {
      if (name.contains(".")) {
        val tokens = name.split("\\.")
        Some((tokens(0), tokens(1)))
      }
      else {
        None
      }
    }

    def apply(n: String, e: String): Unit = {
      fileName = e
      extension = e
    }
  }

  /**
   * Use the Path object to write a variable argument extractor,
   * which extracts the elements of a path in reverse order.
   * Basically implement an unapplySeq method
   * Example:
   * /home/anyuser/development/scala/
   * -> scala development anyuser home
   */
  object Path {
    def unapplySeq(path: String): Option[Seq[String]] = {
      return Some(path.split("/").filter(p => !p.isEmpty).reverse)
    }
  }

  /**
   * Combine the FileName and Path extractor in a match expression to
   * retrieve the filename of a absolute path, e.g.
   * /home/anyuser/development/scala/AdvancedPatternMatchingTest.scala
   * -> AdvancedPatternMatchingTest
   */
  def fileNameRetriever(path: String) = {
    path match {
      case Path(first, _*) =>
        first match {
          case FileName(name, extension) => name
          case _ => "No FileName match"
        }
      case _ => "No Path match"
    }
  }

  /**
   * ***********************************************************************
   * REGEXP MATCHING
   * ***********************************************************************
   */

  /**
   * Define a regexp to match properties the following properties of a performance log-line
   * <date>;PRF;server<number>;<id>;Processing took <amount> ms
   * E.g.:
   * 2010-04-08T04:08:05.889Z;PRF;server1;1004080608005100002;Processing took 200 ms
   */
  val PerfLogLineRE = "(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z).*server(\\d+);(\\d+);.* (\\d+) ms".r

  /**
   * Define a regexp and use the 'loop' function to iterate over a piece of text
   * in order to retrieve phonenumbers in the format: [(]000[)-]0000000. Characters
   * between [] are optional. Example
   * For marketing call 040-2920029, for sales: 0402920029 for finance: (040)2920029
   * -> 040-2920029, 0402920029, (040)2920029
   */
  def phoneNumberRetriever(phoneNumberText: String): List[String] = {
    var result = List[String]()
    for (m <- PhoneNumberRE.findAllIn(phoneNumberText).matchData) {
      result = (m group 1) :: result
    }
    result.reverse
  }

  val PhoneNumberRE = "[^\\d\\(]+([\\(\\d\\)-]+)|(\\d+)\\D*".r

  /**
   * ***********************************************************************
   * XML MATCHING
   * ***********************************************************************
   */


  /*------------------------------------------
   * XML MATCHING HELPER METHODS
   ------------------------------------------*/

  private def getXML = XML.load(this.getClass.getResourceAsStream("/movies.xml"))

  private def movieNodeProcessor(filter: (Node, MList[String]) => Any): List[String] = {
    var capturer = new MList[String]()
    for (movieNode ← getXML \\ "Movie" \ "_") {
      filter(movieNode, capturer)
    }
    capturer.toList
  }

  /**
   * Take a look at the movies.xml. Use xml matching to extract all genres.
   * 	<Movie>
   * 		<Genre>Comedy</Genre>
   * Store the extracted genre-values in a mutable List in order for
   * the unittest to succeed. It is advisable to make use of the
   * movieNodeProcessor method, which functions as a template that loops over all
   * <movie> nodes. Use the function parameter of the movieNodeProcessor
   * method to implement your solution.
   */
  def filterAllGenres(): List[String] = {
    def genreFilter(node: Node,mList: MList[String]): Unit = {
      for (genreNode <- node \\ "Genre") {
        mList += genreNode.text
      }
    }
    movieNodeProcessor(genreFilter)
  }

  /**
   * Take a look at the movies.xml. Use xml matching to extract all actors
   * whose names start with the letter 'G'.
   * 	<Movie>
   * 		...
   *  <Actors>
   *    <Actor>Joseph Fiennes</Actor>
   *    <Actor>Gwyneth Paltrow</Actor>
   *    <Actor>Geoffrey Rush</Actor>
   *  </Actors>
   *  ...
   * Store the extracted actorname-values in a mutable List in order for
   * the unittest to succeed. It is advisable to make use of the
   * movieNodeProcessor method, which functions as a template that loops over all
   * <movie> nodes. Use the function parameter of the movieNodeProcessor
   * method to implement your solution.
   */
  def filterActorsStartingWithG(): List[String] = {
    def gActorFilter(node: Node,mList: MList[String]): Unit = {
      for (gActorNode <- (node \\ "Actors" \\ "Actor").filter(n => n.text.startsWith("G"))) {
        mList += gActorNode.text
      }
    }
    movieNodeProcessor(gActorFilter)
  }

  /**
   * Take a look at the movies.xml. Use xml matching to extract all movies
   * with the top10 attribute set to true.
   * 	<Movie>
   *    <Title top10="true">Ocean's 13</Title>
   *  ...
   * Store the extracted actorname-values in a mutable List in order for
   * the unittest to succeed. It is advisable to make use of the
   * movieNodeProcessor method, which functions as a template that loops over all
   * <movie> nodes. Use the function parameter of the movieNodeProcessor
   * method to implement your solution.
   */
  def filterTop10Titles(): List[String] = {
    def top10Filter(node: Node,mList: MList[String]): Unit = {
      for (top10Node <- node \\ "Title" filter (n => n.attribute("top10").get(0).text == "true")) {
          mList += top10Node.text
      }
    }
    movieNodeProcessor(top10Filter)
  }

  /**
   * Use xml matching to recursively extract all text nodes of a random xml.
   * For the sake of simplicity use the movies.xml file as input xml.
   * Store the extracted text nodes in a mutable List in order for
   * the unittest to succeed. Preferrably, provide your solution in the
   * textNodeMatcher method.
   */
  def recursivelyExtractAllTextNodes(): List[String] = {
    movieNodeProcessor(textNodeMatcher)
  }

  private def textNodeMatcher(nodeSeq: NodeSeq, capturer: MList[String]): Unit = {
    for (node <- nodeSeq filter (n => n.label == "#PCDATA" && !n.text.trim.isEmpty)) capturer += node.text
    for (node <- nodeSeq) textNodeMatcher(node.nonEmptyChildren, capturer)
  }


}