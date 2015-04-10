package kata.roman

import scala.annotation.tailrec

/**
 * RomanNumeralsConverter public interface
 *
 * Converts numbers to and from Roman Numerals
 * @author jon harvey
 */
abstract class RomanNumeralsConverter {

  /**
   * Convert an Integer to Roman Numerals
   *
   * @param number   a number
   * @return romanNumerals
   * @throws IllegalArgumentException if number is not in range: 1-3000
   */
  def toRomanNumerals(number: Int): String

  /**
   * Convert Roman Numerals to an Integer
   *
   * The valid roman numerals are:
   * 'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000
   *
   * @param romanNumerals   a set of Roman numerals, e.g. MDCCCLXXXVIII
   * @return number
   * @throws IllegalArgumentException if roman numerals contains illegal characters or order, is null or empty
   */
  def fromRomanNumerals(romanNumerals: String): Int

}

/**
 * RomanNumeralsConverter implementation
 */
class RomanNumeralsConverterImpl extends RomanNumeralsConverter {

  private val romanNumeralArabPairsList = List(
    ("M", 1000), ("CM", 900),
    ("D", 500), ("CD", 400),
    ("C", 100), ("XC", 90),
    ("L", 50), ("XL", 40),
    ("X", 10), ("IX", 9),
    ("V", 5), ("IV", 4),
    ("I", 1)
  )

  /**
   * 'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000
   */
  private val romanNumeralCharMap: Map[Char, Int] =
    romanNumeralArabPairsList.filter(x => x._1.length == 1).map(x => (x._1.charAt(0), x._2)).toMap

  /**
   * Fold left implementation
   */
  private def toRomanNumeralsFoldLeft(number: Int): String = {
    val (result, _) = romanNumeralArabPairsList.foldLeft("", number) ((result, tuple) => {
      val (romanNumber, arabicNumber) = tuple
      val (romanString, remainder)    = result
      val romanUnit = romanNumber * (remainder / arabicNumber)
      (romanString + romanUnit, remainder % arabicNumber)
    })
    result
  }

  /**
   * Tail Recursive implementation
   */
  private def toRomanNumeralsRecursive(number: Int): String = {
    @tailrec
    def toNumeralsAccumulator(result: String, number: Int, romanNumeralsLeft: List[(String, Int)]): String = {
      if (romanNumeralsLeft.isEmpty)
        result
      else {
        val (roman, arab) = romanNumeralsLeft.head
        toNumeralsAccumulator(result + (roman * (number / arab)) , number % arab, romanNumeralsLeft.tail)
      }
    }
    toNumeralsAccumulator("", number, romanNumeralArabPairsList)
  }

  override def toRomanNumerals(number: Int) = {
    require(number > 0 && number < 3001, s"number: $number is not in range 1-3000")
    if (number % 2 == 0)
      toRomanNumeralsFoldLeft(number)
    else
      toRomanNumeralsRecursive(number)
  }

  override def fromRomanNumerals(romanNumerals: _root_.scala.Predef.String): Int = {
    require(romanNumerals != null && !romanNumerals.isEmpty, "romanNumerals String is null or empty")
    require(romanNumerals.matches("^M{0,3}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$"),
      s"$romanNumerals contains illegal characters or order")
    val (result, _) = romanNumerals.map(romanNumeralCharMap).foldLeft((0,0)) {
      case ((sum, last), curr) =>  (sum + curr + (if (last < curr) -2 * last else 0), curr)
    }
    result
  }

}
