package org.scalalabs.basic.lab03

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

/**
 * @see ForExpressionExercise01
 */
@RunWith(classOf[JUnitRunner])
class ForExpressionExerciseTest extends Specification {

  "ForExpressionExercise01" should {
    "find largest palindrome using a for expression" in {
      ForExpressionExercise01.largestPalindromeWithForExpression(2) === 9009
      ForExpressionExercise01.largestPalindromeWithForExpression(3) === 906609
    }
    "find largest palindrome using a higher order functions" in {
      ForExpressionExercise01.largestPalindromeWithHigherOrderFunctions(2) === 9009
      ForExpressionExercise01.largestPalindromeWithHigherOrderFunctions(3) === 906609
    }
  }

}