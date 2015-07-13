package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
import sys._

object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.sorted.last
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1.isEmpty) {
      l2
    }
    else if (l2.isEmpty) {
      l1
    }
    else {
      (l1 zip l2).foldLeft(List[Int]())((l3,t) => l3 :+ (t._1 + t._2))
    }
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    var result = List[Int]()
    for (intlist <- l) {
      result = sumOfTwo(result, intlist)
    }

    result
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {
    persons
      .partition(person => person.age < 18)
      .productIterator
      .toList
      .map {
        case e:List[Person] =>
          e.map(person => person.firstName)
      }
  }

}