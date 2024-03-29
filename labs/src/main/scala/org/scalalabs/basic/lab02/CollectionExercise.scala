package org.scalalabs.basic.lab02
/**
 * This Lab contains exercises where the usage of
 * higher order collection methods can be rehearsed.
 */
import sys._
import collection.mutable.ListBuffer

object CollectionExercise01 {

  /**
   * Taken from: <a href="http://code.google.com/codejam/contest/1460488/dashboard">Problem A. Speaking in Tongues</a>
   *
   * Problem
   * The aim of this task is to translate a language into a new language called Googlerese.
   * To translate we take any message and replace each English letter with another English letter.
   * This mapping is one-to-one and onto, which means that the same input letter always gets replaced
   * with the same output letter, and different input letters always get replaced with different output letters.
   * A letter may be replaced by itself. Spaces are left as-is.
   *
   * For example (and here is a hint!), the translation algorithm includes the following three mappings:
   * 'a' -> 'y', 'o' -> 'e', and 'z' -> 'q'. This means that "a zoo" will become "y qee".
   *
   * Sample Input/Output
   * Input:
   *
   * Output:
   * Case 1: ejp mysljylc kd kxveddknmc re jsicpdrysi
   * Case 1: our language is impossible to understand
   *
   * Case 2: rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd
   * Case 2: there are twenty six factorial possibilities
   *
   * Case 3: de kr kd eoya kw aej tysr re ujdr lkgc jv
   * Case 3: so it is okay if you want to just give up
   *
   */

  // 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
  // a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
  //             o              u        l        r        n                 a




  def translateString(line: String):String = {
    val translation: Map[Char, Char] = Map(
      ' ' -> ' ',
      'a' -> 'y', // 1 -> 25
      'b' -> 'h', // 2 -> 8
      'c' -> 'e', // 3 -> 5
      'd' -> 's', // 4 -> 19
      'e' -> 'o', // 5 ->
      'f' -> 'c',
      'g' -> 'v',
      'h' -> 'x',
      'i' -> 'd',
      'j' -> 'u',
      'k' -> 'i',
      'l' -> 'g',
      'm' -> 'l',
      'n' -> 'b',
      'o' -> 'k',
      'p' -> 'r',
      'q' -> 'z',
      'r' -> 't',
      's' -> 'n',
      't' -> 'w',
      'u' -> 'j',
      'v' -> 'p',
      'w' -> 'f',
      'x' -> 'm',
      'y' -> 'a',
      'z' -> 'q'
    )

    line.flatMap(c => translation.get(c))
  }

  def googleCodeJamGooglerese(lines: String*): Seq[String] = {
    List.tabulate(lines.length)(n =>
      translateString(lines(n))
    )
  }
}
/*========================================================== */

object CollectionExercise02 {

  class Person(val age: Int, val name: String)

  /**
   * Take a look at the java class: {@link ImperativeSample}. The
   * groupAdultsPerAgeGroup is implemented using an imperative programming
   * style.
   * Rewrite the method groupAdultsPerAgeGroup in the ImperativeSample java class
   * using a functional approach.
   */
  def groupAdultsPerAgeGroup(persons: Seq[Person]): Map[Int, Seq[Person]] = {
    persons.sortBy(_.name)
      .map(c => (c.age - (c.age % 10), c))
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .map {
        case (k,v) => (k,v.map(_._2))
      }
      .toMap
  }
}

/*========================================================== */

object CollectionExercise03 {
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease[T <% Ordered[T]](seq: Seq[T]): Boolean =
    if (seq.length == 1) true
    else seq.sliding(2).toList.foldLeft(true)((b, a) => b && (a(0) < a(1)))
}
/*========================================================== */

object CollectionExercise04 {
  /**
   * Calculate the length of the longest word in a list of sentences.
   * To keep it simple it's ok to use String.split to extract all words of a sentence.
   */
  def calcLengthLongestWord(lines: String*): Int = {
    lines.mkString(" ")
      .split("\\s+")
      .foldLeft(0)((b,word) => {
        if (word.length > b) {
          word.length
        }
        else {
          b
        }
      })
  }
}

/*========================================================== */

object CollectionExercise05 {
  /**
   * Filter all even numbers of the given sequence using foldLeft.
   * E.g. Seq(1,2,3) is Seq(2)
   */
  def filterWithFoldLeft(seq: Seq[Int]): Seq[Int] = {
    seq.foldLeft(Seq[Int]())((b,x) => if (x % 2 == 0) b :+ x else b)
  }

  /**
   * Group all numbers based on whether they are even or odd using foldLeft.
   * For even use 'true' for odd use 'false'.
   * E.g: Seq(1,2,3) is Map(true -> Seq(2), false -> Seq(1,3))
   */
  def groupByWithFoldLeft(seq: Seq[Int]): Map[Boolean, Seq[Int]] = {
    seq.foldLeft(Map[Boolean, Seq[Int]]())((map, x) => {
      val isEven = x % 2 == 0
      map + (isEven -> (map.getOrElse(isEven, Seq[Int]()) :+ x))
    })
  }
}

