package kata

/**
 * Converts numbers to and from Roman Numerals
 * @author jon harvey
 */
object RomanNumeralsConverter {

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
    romanNumeralArabPairsList.filter(x => (x._1.length == 1)).map(x => (x._1.charAt(0), x._2)).toMap


  /**
   * Return an Integer in Roman Numerals
   *
   * @param number
   * @return romanNumerals
   * @throws IllegalArgumentException if number is not in range: 1-3000
   */
  def toRomanNumerals(number: Int): String = {
    require(number > 0 && number < 3001, s"number: $number is not in range 1-3000")
    toRomanNumeralsFoldLeft(number)
  }

  /**
   * Fold left implementation
   *
   * @param number
   * @return romanNumerals
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
   * Recursive implementation
   * @param number
   * @return
   */

  private def toRomanNumeralsRecursive(number: Int): String = {
    def toNumeralsAccumulator(number: Int, romanNumeralsLeft: List[(String, Int)]): String = romanNumeralsLeft.headOption match {
      case None => ""
      case Some((roman, arab)) => roman * (number / arab) + toNumeralsAccumulator(number % arab, romanNumeralsLeft.tail)
    }
    toNumeralsAccumulator(number, romanNumeralArabPairsList)
  }

  /**
   * From Roman Numerals return an Integer
   *
   * The valid roman numerals are:
   * 'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000
   *
   * @param romanNumerals
   * @return number
   * @throws IllegalArgumentException if roman numerals contains illegal characters
   */
  def fromRomanNumerals(romanNumerals: String): Int = {
    require(romanNumerals != null && !romanNumerals.isEmpty, "romanNumerals String is null or empty")
    require(romanNumerals.filterNot(romanNumeralCharMap.keySet).isEmpty, s"$romanNumerals contains illegal characters")
    val (result, _) = romanNumerals.map(romanNumeralCharMap).foldLeft((0,0)) {
      case ((sum, last), curr) =>  (sum + curr + (if (last < curr) -2 * last else 0), curr)
    }
    result
  }

}
