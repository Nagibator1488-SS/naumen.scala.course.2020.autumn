import Exercises.Vector2D
import utest._

object Test extends TestSuite{

  val ballsForTest1: Map[String, (Int, Double)] =
    Map("Aluminum" -> (3,   2.6889))
  val ballsForTest2: Map[String, (Int, Double)] =
    Map("Graphite" ->  (12,  2.1), "Aluminum" -> (0,   2.6889), "Tungsten" ->  (2,   0))
  val ballsForTest3: Map[String, (Int, Double)] =
    Map(
      "Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35), "Graphite" ->  (12,  2.1),   "Iron" ->      (3,   7.874),
      "Gold" ->     (2,   19.32),  "Potassium" -> (14,  0.862), "Calcium" ->   (8,   1.55),  "Cobalt" ->    (4,   8.90),
      "Lithium" ->  (12,  0.534),  "Magnesium" -> (10,  1.738)
    )
  val ballsForTest4: Map[String, (Int, Double)] =
    Map(
      "Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35), "Graphite" ->  (12,  2.1),   "Iron" ->      (3,   7.874),
      "Gold" ->     (2,   19.32),  "Potassium" -> (14,  0.862), "Calcium" ->   (8,   1.55),  "Cobalt" ->    (4,   8.90),
      "Lithium" ->  (12,  0.534),  "Magnesium" -> (10,  1.738), "Copper" ->    (3,   8.96),  "Sodium" ->    (5,   0.971),
      "Nickel" ->   (2,   8.91),   "Tin" ->       (1,   7.29),  "Platinum" ->  (1,   21.45), "Plutonium" -> (3,   19.25),
      "Lead" ->     (2,   11.336), "Titanium" ->  (2,   10.50), "Silver" ->    (4,   4.505), "Uranium" ->   (2,   19.04),
      "Chrome" ->   (3,   7.18),   "Cesium" ->    (7,   1.873), "Zirconium" -> (3,   6.45)
    )

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(5, 5) == Seq())
      assert(Exercises.divBy3Or7(6, 6) == Seq(6))
      assert(Exercises.divBy3Or7(6, 14) == Seq(6, 7, 9, 12, 14))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
      assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
      assert(Exercises.sumOfDivBy3Or5(-8, 0) == -14)
      assert(Exercises.sumOfDivBy3Or5(10, 20) == 75)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(10) == Seq(2, 5))
      assert(Exercises.primeFactor(2) == Seq(2))
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(1024) == Seq(2))
      assert(Exercises.primeFactor(20) == Seq(2, 5))
      assert(Exercises.primeFactor(200) == Seq(2, 5))
      assert(Exercises.primeFactor(7369) == Seq(7369))
      assert(Exercises.primeFactor(2310) == Seq(2, 3, 5, 7, 11))
    }
    'test_sumScalars - {
      assert(Exercises.sumScalars(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0)) == 16)
      assert(Exercises.sumScalars(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(3.0, 2.0), Vector2D(3.0, 2.0)) == 21)
      assert(Exercises.sumScalars(Vector2D(3.0, 5.0), Vector2D(1.0, 3.0), Vector2D(4.0, 10.0), Vector2D(3.0, 5.0)) == 80)
      assert(Exercises.sumScalars(Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0)) == 0)
      assert(Exercises.sumScalars(Vector2D(2.145, 2.234), Vector2D(2.228, 2.123), Vector2D(2.199, 2.897), Vector2D(2.77777, 2.98789)) == 24.286075559999997)
      assert(Exercises.sumScalars(Vector2D(-2.0, -3.0), Vector2D(-4.0, -5.123), Vector2D(-2.0, -1.124), Vector2D(-22.3, 2.0)) == 65.721)
    }
    'test_sumCosines - {
      assert(Exercises.sumCosines(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0)) == 1.9999999999999998)
      assert(Exercises.sumCosines(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(3.0, 2.0), Vector2D(3.0, 2.0)) == 2.0)
      assert(Exercises.sumCosines(Vector2D(3.0, 5.0), Vector2D(1.0, 3.0), Vector2D(4.0, 10.0), Vector2D(3.0, 5.0)) == 1.9634281808965999)
      assert(Exercises.sumCosines(Vector2D(2.145, 2.234), Vector2D(2.228, 2.123), Vector2D(2.199, 2.897), Vector2D(2.77777, 2.98789)) == 1.9940467771497488)
      assert(Exercises.sumCosines(Vector2D(-2.0, -3.0), Vector2D(-4.0, -5.123), Vector2D(-2.0, -1.124), Vector2D(-22.3, 2.0)) == 1.821707518082242)
    }
    'test_sortByHeavyweight - {
      assert(Exercises.sortByHeavyweight(ballsForTest1) == Seq("Aluminum"))
      assert(Exercises.sortByHeavyweight(ballsForTest2) == Seq("Aluminum", "Tungsten", "Graphite"))
      assert(Exercises.sortByHeavyweight(ballsForTest3) == Seq("Aluminum", "Iron", "Lithium", "Gold", "Tungsten", "Calcium", "Cobalt", "Potassium", "Magnesium", "Graphite"))
      assert(Exercises.sortByHeavyweight(ballsForTest4) == Seq("Tin", "Platinum", "Aluminum", "Sodium", "Nickel", "Titanium", "Lead", "Zirconium", "Chrome", "Iron", "Silver", "Uranium", "Lithium", "Gold", "Tungsten", "Copper", "Cesium", "Calcium", "Cobalt", "Potassium", "Plutonium", "Magnesium", "Graphite"))
      assert(Exercises.sortByHeavyweight(Map()) == Seq())
    }
  }
}
