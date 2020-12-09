import Exercises.wordReverse

object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  def fibonacci4Index(idx: Int): Int =
    if (idx == 0)
      0
    else if (idx == 1 || idx == 2)
      1
    else
      fibonacci4Index(idx - 2) + fibonacci4Index(idx - 1)

  def fibonacci(idx: Int): Seq[Int] = idx match {
    case 0 => Seq(0)
    case 1 => Seq(0, 1)
    case 2 => Seq(0, 1, 1)
    case _ => fibonacci(idx - 1) :+ (fibonacci(idx - 2).last + fibonacci(idx - 1).last)
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String =
    text.toUpperCase.split("").map(symbol =>
      if (MORSE.contains(symbol))
        MORSE(symbol)
      else symbol).mkString(" ")


  def wordReverse(text: String): String =
    text
      .split("(?=[!. ,?])|(?<=[!. ,?])")
      .map(word => if (word.charAt(0).isUpper) {
        word.toLowerCase.reverse.capitalize
      } else word.reverse)
      .mkString("")
}
object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(wordReverse("Зима!.. Крестьянин, торжествуя..."))
  }
}