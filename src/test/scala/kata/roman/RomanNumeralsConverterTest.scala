package kata.roman

import org.specs2.execute.Result
import org.specs2.mutable.Specification

/**
 * Unit test using specs2
 */
class RomanNumeralsConverterTest extends Specification {

  /**
   * map of example valid integers to roman numerals
   */
  private val validNumberRomanNumerals = Map(
    1745 -> "MDCCXLV", 33 -> "XXXIII", 45 -> "XLV",
    1999 -> "MCMXCIX", 3000 -> "MMM", 1888 -> "MDCCCLXXXVIII")

  /**
   * converter implementation to test
   */
  private val converter: RomanNumeralsConverter = new RomanNumeralsConverterImpl

  "The RomanNumeralsConverter.toRomanNumerals" should {
    for (number <- List(-1, 0, 30001)) {
      s"throw an IllegalArgumentException 'number: $number is not in range 1-3000' for " + number in {
        converter.toRomanNumerals(number) must
          throwA[IllegalArgumentException](s"number: $number is not in range 1-3000")
      }
    }
    for ((k,v) <- validNumberRomanNumerals) {
      s"convert number: $k to roman numerals: $v" in {
        converter.toRomanNumerals(k) mustEqual v
      }
    }
  }

  "The RomanNumeralsConverter.fromRomanNumerals" should {
    val nullEmptyMessage = "romanNumerals String is null or empty"
    s"throw an IllegalArgumentException '$nullEmptyMessage' for null" in {
      converter.fromRomanNumerals(null) must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    s"throw an IllegalArgumentException '$nullEmptyMessage' for empty String" in {
      converter.fromRomanNumerals("") must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    for (romanNumeral <- List("A", "B", "Z", "BII")) {
      s"throw an IllegalArgumentException '$romanNumeral contains illegal characters' for illegal characters like " +
        romanNumeral in {
        converter.fromRomanNumerals(romanNumeral) must
          throwA[IllegalArgumentException](s"$romanNumeral contains illegal characters")
      }
    }
    for ((k,v) <- validNumberRomanNumerals) {
      s"convert roman numerals: $v to number: $k" in {
        converter.fromRomanNumerals(v) mustEqual k
      }
    }
  }

  "The RomanNumeralsConverter.toRomanNumerals & then fromRomanNumerals" should {
    "result in the original number for numbers 1 - 3000" in {
      Result.unit { (1 to 3000) foreach { number => number mustEqual
        converter.fromRomanNumerals(converter.toRomanNumerals(number))} }
    }
  }

}
