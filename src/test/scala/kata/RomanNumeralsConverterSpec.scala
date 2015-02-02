package kata

import org.specs2.execute.Result
import org.specs2.mutable.Specification

/**
 * RomanNumeralsConverterSpec
 */
class RomanNumeralsConverterSpec extends Specification {

  private val validNumberRomanNumerals = Map(
    1745 -> "MDCCXLV", 33 -> "XXXIII", 45 -> "XLV",
    1999 -> "MCMXCIX", 3000 -> "MMM", 1888 -> "MDCCCLXXXVIII")

  "The RomanNumeralsConverter.toRomanNumerals" should {
    for (number <- List(-1, 0, 30001)) {
      s"throw an IllegalArgumentException 'number: $number is not in range 1-3000' for " + number in {
        RomanNumeralsConverter.toRomanNumerals(number) must
          throwA[IllegalArgumentException](s"number: $number is not in range 1-3000")
      }
    }
    for ((k,v) <- validNumberRomanNumerals) {
      s"convert number: $k to roman numerals: $v" in {
        RomanNumeralsConverter.toRomanNumerals(k) mustEqual v
      }
    }
  }

  "The RomanNumeralsConverter.fromRomanNumerals" should {
    val nullEmptyMessage = "romanNumerals String is null or empty"
    s"throw an IllegalArgumentException '$nullEmptyMessage' for null" in {
      RomanNumeralsConverter.fromRomanNumerals(null) must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    s"throw an IllegalArgumentException '$nullEmptyMessage' for empty String" in {
      RomanNumeralsConverter.fromRomanNumerals("") must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    for (romanNumeral <- List("A", "B", "Z", "BII")) {
      s"throw an IllegalArgumentException '$romanNumeral contains illegal characters' for illegal characters like " +
        romanNumeral in {
        RomanNumeralsConverter.fromRomanNumerals(romanNumeral) must
          throwA[IllegalArgumentException](s"$romanNumeral contains illegal characters")
      }
    }
    for ((k,v) <- validNumberRomanNumerals) {
      s"convert roman numerals: $v to number: $k" in {
        RomanNumeralsConverter.fromRomanNumerals(v) mustEqual k
      }
    }
  }

  "The RomanNumeralsConverter.toRomanNumerals & then fromRomanNumerals" should {
    "result in the original number for numbers 1 - 3000" in {
      Result.unit { (1 to 3000) foreach { number => number mustEqual
        RomanNumeralsConverter.fromRomanNumerals(RomanNumeralsConverter.toRomanNumerals(number))} }
    }
  }

}
