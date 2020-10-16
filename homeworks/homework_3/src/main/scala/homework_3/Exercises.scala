package homework_3

object Exercises {
  def prettyBooleanFormatter1(x: Any): String = {
    if (x.isInstanceOf[Boolean])
      if (x.asInstanceOf[Boolean])
        return "правда"
      else
        return "ложь"
    else
      return x.toString
  }

  def prettyBooleanFormatter2(x: Any): String = {
    if(x.getClass == Class.forName("java.lang.Boolean"))
      if (x.asInstanceOf[Boolean])
        return "правда"
      else
        return "ложь"
    else
      return x.toString
  }

  def prettyBooleanFormatter3(x: Any): String = {
    x match {
      case true => "правда"
      case false => "ложь"
      case _ => x.toString
    }
  }

  def max1(xs: Seq[Int]): Int = {
    if (xs.isEmpty)
      throw new UnsupportedOperationException
    return xs.max
  }

  def max2(xs: Seq[Int]): Seq[Int] = {
    if (xs.isEmpty)
      return Seq()
    return Seq(xs.max)
  }

  def max3(xs: Seq[Int]): Option[Int] = {
    xs match {
      case Seq() => None
      case _ => Option(xs.max)
    }
  }

  def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum

  def sum1(x: Int, y: Int): Int = sumIntegers(List(x, y))

  def sum2(x: Int, y: Int): Int = sumIntegers(Array(x,y).toIterable)

  def sum3(x: Int, y: Int): Int = sumIntegers(MyIterableClass(x, y))
}

case class MyIterableClass(x: Int, y: Int) extends Iterable[Int]{
  override def iterator:Iterator[Int] = Iterator(x,y)
}
