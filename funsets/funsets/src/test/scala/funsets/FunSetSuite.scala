package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(1)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

/*  test("union all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect all elements of each set") {
    new TestSets {
      val s = intersect(s1, s4)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 3), "Intersect 3")
      assert(!contains(intersect(s1, s2), 1), "intersect(s1, s2)")
    }
  }

  test("diff all elements of each set") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "diff 1")
      assert(contains(s, 2), "diff 2")
      assert(!contains(diff(s1, s4), 1), "diff(s1, s4)")
      val s_ = diff(s2, s1)
      assert(contains(s_, 1), "diff 3")
      assert(contains(s_, 2), "diff 4")

      val s_s1 = union(union(union(union(s1, s3), singletonSet(5)), singletonSet(7)), singletonSet(1000))
      val s_s2 = union(union(union(s1, s2), s3), singletonSet(4))
      val dif = diff(s_s1, s_s2)
      printSet(dif)
      assert(!contains(dif, 2), "diff 5")
    }
  }


  test("filter all elements of each set") {
    new TestSets {
      val s = filter(union(s1,s2), u => (u == 1))
     // printSet(s)
      assert(contains(s, 1), "filter 1")
      assert(!contains(s, 2), "filter 2")
    }
  }

  test("forall all elements with predicate") {
    new TestSets {
      assert(forall(s1, u => (u == 1)), "forall 1")
      assert(!forall(union(s1,s2), u => (u == 1)), "forall 2")
      assert(!forall(s3, u => (u == 1)), "forall 3")
      assert(!forall(s1, u => false), "forall 4")
    }
  }


  test("exists one elements of each set") {
    new TestSets {
      assert(exists(s1, u => (u == 1)), "exists 1")
      assert(!exists(s2, u => (u == 1)), "exists 2")
      assert(exists(union(s1, s2), u => (u == 2)), "exists union")
    }
  }*/

  test("map contains only one value") {
    new TestSets {
     val m1 = map(s3, u => 1)
      assert(contains(m1, 1), "map 1")
      //assert(!contains(m1, 3), "not map 1")
    //  val m2 = map(union(s1, s3), u => 2)
    //  assert(contains(m2, 2), "map 2")
    //  assert(!contains(m2, 1), "map 3")
    }
  }

}
