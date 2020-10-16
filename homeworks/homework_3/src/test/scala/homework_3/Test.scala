package homework_3

import utest._

object Test extends TestSuite{
  val trueStr = "правда"
  val falseStr = "ложь"
  val tests = Tests{
    'test_prettyBooleanFormatter1 - {
      assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter1(1543) == "1543")
      assert(Exercises.prettyBooleanFormatter1("GoodJob") == "GoodJob")
      assert(Exercises.prettyBooleanFormatter1(Seq(1, 2, "GoodJob")) == "List(1, 2, GoodJob)")
      assert(Exercises.prettyBooleanFormatter1(Map("a" -> "b")) == "Map(a -> b)")
    }
    'test_prettyBooleanFormatter2 - {
      assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter2(1543) == "1543")
      assert(Exercises.prettyBooleanFormatter2("GoodJob") == "GoodJob")
      assert(Exercises.prettyBooleanFormatter2(Seq(1, 2, "GoodJob")) == "List(1, 2, GoodJob)")
      assert(Exercises.prettyBooleanFormatter2(Map("a" -> "b")) == "Map(a -> b)")
    }
    'test_prettyBooleanFormatter3 - {
      assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter3(1543) == "1543")
      assert(Exercises.prettyBooleanFormatter3("GoodJob") == "GoodJob")
      assert(Exercises.prettyBooleanFormatter3(Seq(1, 2, "GoodJob")) == "List(1, 2, GoodJob)")
      assert(Exercises.prettyBooleanFormatter3(Map("a" -> "b")) == "Map(a -> b)")
    }
    'test_max1 - {
      intercept[UnsupportedOperationException] {Exercises.max1(Seq())}
      assert(Exercises.max1(Seq(1)) == 1)
      assert(Exercises.max1(Seq(1, 2, 3)) == 3)
      assert(Exercises.max1(Seq(1, 2, 10, 3)) == 10)
      assert(Exercises.max1(Seq(-1, -2, -3)) == -1)
      assert(Exercises.max1(Seq(-1, -2, -3, 0)) == 0)
    }
    'test_max2 - {
      assert(Exercises.max2(Seq()) == Seq())
      assert(Exercises.max2(Seq(1)) == Seq(1))
      assert(Exercises.max2(Seq(1, 2, 3)) == Seq(3))
      assert(Exercises.max2(Seq(1, 2, 10, 3)) == Seq(10))
      assert(Exercises.max2(Seq(-1, -2, -3)) == Seq(-1))
      assert(Exercises.max2(Seq(-1, -2, -3, 0)) == Seq(0))
    }
    'test_max3 - {
      assert(Exercises.max3(Seq()) == None)
      assert(Exercises.max3(Seq(1)) == Some(1))
      assert(Exercises.max3(Seq(1, 2, 3)) == Some(3))
      assert(Exercises.max3(Seq(1, 2, 10, 3)) == Some(10))
      assert(Exercises.max3(Seq(-1, -2, -3)) == Some(-1))
      assert(Exercises.max3(Seq(-1, -2, -3, 0)) == Some(0))
    }
    'test_sum1 - {
      assert(Exercises.sum1(1, 2) == 3)
      assert(Exercises.sum1(-11, 222) == 211)
      assert(Exercises.sum1(89, 0) == 89)
      assert(Exercises.sum1(-15, -90) == -105)
    }
    'test_sum2 - {
      assert(Exercises.sum2(1, 2) == 3)
      assert(Exercises.sum2(-11, 222) == 211)
      assert(Exercises.sum2(89, 0) == 89)
      assert(Exercises.sum2(-15, -90) == -105)
    }
    'test_sum3 - {
      assert(Exercises.sum3(1, 2) == 3)
      assert(Exercises.sum3(-11, 222) == 211)
      assert(Exercises.sum3(89, 0) == 89)
      assert(Exercises.sum3(-15, -90) == -105)
    }
  }
}
