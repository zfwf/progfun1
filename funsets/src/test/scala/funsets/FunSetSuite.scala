package funsets

import org.junit._

/**
  * This class is a test suite for the methods in object FunSets.
  *
  * To run this test suite, start "sbt" then run the "test" command.
  */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
  }

  /**
    * This test is currently disabled (by using @Ignore) because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", remvoe the
    * @Ignore annotation.
    */
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `diff contains all elements from first set but not in the second set`
      : Unit = {
    new TestSets {
      val i1 = diff(s1, s1)
      assert(!contains(i1, 1), "diff(1, 1)")
      val i2 = diff(s1, s2)
      assert(contains(i2, 1), "diff(1, 2)")
      val i3 = diff(s1, s2)
      assert(!contains(i3, 2), "diff(1, 2)")
    }
  }

  @Test def `filter contains all elements that satisfy the predicate`: Unit = {
    new TestSets {
      val f1 = filter(s1, (x: Int) => x == 1)
      assert(contains(f1, 1), "filter s1")
      val f2 = filter(s1, (x: Int) => x == 2)
      assert(!contains(f2, 2), "filter s2")
    }
  }

  @Test def `intersect contains all elements that appeared in both sets`
      : Unit = {
    new TestSets {
      val i1 = intersect(s1, s1)
      assert(contains(i1, 1), "intersect s1 with itself")
      val i2 = intersect(s1, s2)
      assert(!contains(i2, 2), "no intersect between s1 and s2")
    }
  }

  @Test def `forall verifies all elements in set satisfy predicate p`: Unit = {
    new TestSets {
      assert(forall(s1, (x: Int) => x == 1), "forall s1 1")
      assert(!forall(s2, (x: Int) => x == 1), "forall s2 1")
    }
  }

  @Test def `exists verifies at least one elements in set satisfy predicate p`
      : Unit = {
    new TestSets {
      assert(exists(s1, (x: Int) => x == 1), "exists s1 1")
      assert(!exists(s2, (x: Int) => x == 1), "exists s2 1")
    }
  }

  @Test def `map modifies every element of the set`: Unit = {
    new TestSets {
      assert(forall(map(s1, (x: Int) => 1), s1), "map s1 to s1")
      assert(forall(map(s2, (x: Int) => 1), s1), "map s2 to s1")
      assert(forall(map(s3, (x: Int) => 2), s2), "map s3 to s2")
    }
  }
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
