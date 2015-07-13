package org.scalalabs.basic.lab03

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

/**
 * @see RecursionPatternMatchingExercise
 */
@RunWith(classOf[JUnitRunner])
class RecursionPatternMatchingExerciseTest extends Specification {

  "RecursionPatternMatchingExercise" should {
    "check subsequent values increase" in {
      RecursionPatternMatchingExercise.checkValuesIncrease(List(1, 2, 3, 5, 10)) === true
      RecursionPatternMatchingExercise.checkValuesIncrease(List(1)) === true
      RecursionPatternMatchingExercise.checkValuesIncrease(List(1, 2, 2, 5, 10)) === false
      RecursionPatternMatchingExercise.checkValuesIncrease(List(1, 2, 2, 5, 1)) === false
    }
    "group consecutive members" in {
      RecursionPatternMatchingExercise.groupConsecutive(List(1, 1, 1, 5, 4, 4, 1, 1)) must
        containTheSameElementsAs(List(List(1, 1, 1), List(5), List(4, 4), List(1, 1)))
    }
    "group equal members" in {
      RecursionPatternMatchingExercise.groupEquals(List(1, 1, 1, 5, 4, 4, 5, 1)) must
        containTheSameElementsAs(List(List(1,1,1,1), List(4,4), List(5,5)))
    }
    "remove consecutive duplicates" in {
      RecursionPatternMatchingExercise.compress(List(1, 1, 1, 1, 5, 8, 8, 4, 4, 4, 9, 9)) ===
        List(1, 5, 8, 4, 9)
    }
    "define amount equal members" in {
      RecursionPatternMatchingExercise.amountEqualMembers(List('x, 'x, 'x, 'y, 'z, 'z, 'y, 'x)) must
        containTheSameElementsAs(List((4, 'x), (2, 'y), (2, 'z)))
      RecursionPatternMatchingExercise.amountEqualMembers(List("Cow", "Cow", "Boy", "Cow", "Boy", "Hut", "Cow")) must
        containTheSameElementsAs(List((4, "Cow"), (2, "Boy"), (1, "Hut")))
    }
    "zip multiple" in {
      RecursionPatternMatchingExercise.zipMultiple(List(List(1, 2, 3), List('A, 'B, 'C), List('a, 'b, 'c))) ===
        List(List(1, 'A, 'a), List(2, 'B, 'b), List(3, 'C, 'c))
    }
    "zip multiple with different size" in {
      RecursionPatternMatchingExercise.zipMultipleWithDifferentSize(List(List(1, 2), List('A, 'B, 'C), List('a))) ===
        List(List(1, 'A, 'a))
    }
  }
}