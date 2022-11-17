package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite {

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
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)

  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      val us = union(s3,s)
      assert(contains(us,3))
    }
  }

  test("Intersect contains elements belonging to both sets"){
    new TestSets {
      val inter = intersect(s1,s2)
      assert(!contains(inter,1))
      assert(!contains(inter,2))
      val s = union(s1, s2)
      val inter2 = intersect(s1,s)
      assert(contains(inter2,1))
    }
  }

  test("dif contains elements that are in the first set but not the second") {
    new TestSets {
      val dif1 = diff(s1, s2)
      val s = union(s1, s2)
      val dif2 = diff(s1, s)
      val dif3 = diff(s1,s1)
      val dif4 = diff(s1,s2)
      assert(contains(dif1, 1))
      assert(!contains(dif1, 2))
      assert(!contains(dif2,1)) // 1 is in union and s1 so set dif2 should be empty
      assert(!contains(dif3,1))
      assert(contains(dif4,1) && !contains(dif4,2))
    }
  }

  test("filter should create a subset based on some predicate") {
    new TestSets {
      val p1: Int => Boolean = x => if(x > 1) true else false
      val p2: Int => Boolean = x => if(x < 3) true else false
      def p3(p1: Int => Boolean,p2: Int => Boolean): Int => Boolean = x => if(p1(x) && p2(x)) true else false
      val s = union(union(s1,s2),s3)
      assert(contains(filter(s,p1), 2))
      assert(contains(filter(s,p1), 3))
      assert(!contains(filter(s,p1), 1))

      assert(contains(filter(s,p2), 2))
      assert(!contains(filter(s,p2), 3))
      assert(contains(filter(s,p2), 1))

      assert(contains(filter(s,p3(p1, p2)),2))
      assert(!contains(filter(s,p3(p1, p2)),1))
      assert(!contains(filter(s,p3(p1, p2)),3))

      //now test numbers not in s
      assert(!contains(filter(s,p3(p1, p2)),0))
      assert(!contains(filter(s,p3(p1, p2)),4))
    }
  }

  test("forall should give true if all s satisfies p"){
    new TestSets{
      val p1: Int => Boolean = x => if (x > 0) true else false
      val p2: Int => Boolean = x => if (x > 1) true else false
      val p3: Int => Boolean = x => if (x > 4) true else false
      val s = union(union(s1, s2), s3)
      assert(forall(s,p1))
      assert(!forall(s,p2)) //Some elements of s do not satisfy p
      assert(forall(intersect(s1,s2),p1)) //This should always be true regardless of what p1 is (i.e. set is empty)
      assert(!forall(s,p3)) // no value in s satisfies p
    }
  }

  test("exists should give true if at least one element of s satisfies p and false if none do"){
    new TestSets{
      val p1: Int => Boolean = x => if (x > 0) true else false
      val p2: Int => Boolean = x => if (x > 1) true else false
      val p3: Int => Boolean = x => if (x > 4) true else false
      val s = union(union(s1, s2), s3)
      assert(exists(s,p1))
      assert(contains(diff(s,p2),1))
      assert(!contains(diff(p2,s),1))
      assert(!contains(diff(p2,s),2))
      assert(!contains(diff(p2,s),3))
      assert(!contains(intersect(diff(s,p2),diff(p2,s)),1))
      assert(forall(intersect(diff(s,p2),diff(p2,s)),p1))
      assert(exists(s,p2))
      assert(!exists(intersect(s1,s2),p1))
      assert(!exists(s,p3))
    }
  }


  test("test various functions for map"){
    new TestSets {
      val f1: Int => Int = x => x * 2
      val f2: Int => Int = x => x + 5
      val f3: Int => Int = x => -x
      val p1: Int => Boolean = x => if(x > 0) true else false
      val s = union(s1,union(s2,s3))
      assert(contains(map(s,f1),2))
      assert(contains(map(s,f1),4))
      assert(contains(map(s,f1),6))

      assert(!contains(map(s,f1),1))
      assert(!contains(map(s,f1),3))

      assert(contains(map(s,f2),6))
      assert(contains(map(s,f2),7))
      assert(contains(map(s,f2),8))
      assert(forall(map(s,f1),p1))
    }
  }


  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
