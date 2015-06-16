package kata.roman

import org.scalatest._
import RomanNumeralsConverterImpl._

/**
 * FlatSpec using scalatest
 */
class RomanNumeralsConverterSpec extends FlatSpec with Matchers {
markup { """
## RomanNumeralsConverter

The RomanNumeralsConverter has two operations:

* __toRomanNumerals__ to convert a number, e.g. 33 to RomanNumerals, e.g. XXXIII
    * An _IllegalArgumentException_ is thrown if the number is not in the range 1-3000

* __fromRomanNumerals__ to convert a romanNumeral, e.g. XXXIII to a number, e.g. 33
    * An _IllegalArgumentException_ is thrown if the roman numerals:
        * is empty or null
        * contains illegal characters, .i.e. not I, V, X, L, C, D, M
        * the order of characters is incorrect

"""}

  /**
   * converter implementations to test
   */
  private val converters: Set[RomanNumeralsConverter] = Set(
    RomanNumeralsConverterImpl(Recursive),
    RomanNumeralsConverterImpl(FoldLeft))

  /**
   * valid numbers are in the range 1-3000,
   * some example invalid numbers
   */
  private val invalidNumbers = Set(-1, 0, 3001)

  /**
   * map of example valid integers to roman numerals
   */
  private val validNumberRomanNumerals = Map(
    1745 -> "MDCCXLV", 33 -> "XXXIII", 45 -> "XLV",
    1999 -> "MCMXCIX", 3000 -> "MMM", 1888 -> "MDCCCLXXXVIII")

  "The RomanNumeralsConverter.toRomanNumerals" should
  "throw IllegalArgumentException for numbers not in the range 1-3000: "+invalidNumbers in {
    for (converter <- converters; invalid <- invalidNumbers)
      an [IllegalArgumentException] should be thrownBy {
        converter.toRomanNumerals(invalid)
      }
  }

  it should "convert some valid numbers: "+validNumberRomanNumerals.keys in {
    for (converter <- converters; (k, v) <- validNumberRomanNumerals) {
      converter.toRomanNumerals(k) shouldBe v
    }
  }

  val nullEmptyMessage = "requirement failed: romanNumerals String is null or empty"

  "The RomanNumeralsConverter.fromRomanNumerals" should
  s"throw IllegalArgumentException '$nullEmptyMessage' for null" in {
    the [IllegalArgumentException] thrownBy {
      for (converter <- converters)
        converter.fromRomanNumerals(null)
    } should have message nullEmptyMessage
  }

  it should s"throw IllegalArgumentException '$nullEmptyMessage' for empty String" in {
    the [IllegalArgumentException] thrownBy {
      for (converter <- converters)
        converter.fromRomanNumerals("")
    } should have message nullEmptyMessage
  }

  val invalidRomanNumerals = Set("A", "Z", "IIII", "VXLV", "LXC", "XDCC", "CCMM")

  it should "throw IllegalArgumentException for invalid roman numerals: "+invalidRomanNumerals in {
    for (romanNumeral <- invalidRomanNumerals) {
      the [IllegalArgumentException] thrownBy {
        for (converter <- converters)
          converter.fromRomanNumerals(romanNumeral)
      } should have message s"requirement failed: $romanNumeral contains illegal characters or order"
    }
  }

  it should "convert some valid roman numerals: "+validNumberRomanNumerals.values.toSet in {
    for (converter <- converters; (k, v) <- validNumberRomanNumerals) {
      converter.fromRomanNumerals(v) shouldBe k
    }
  }

  "The RomanNumeralsConverter.toRomanNumerals & then fromRomanNumerals" should
  "result in the original number for numbers 1-3000" in {
    for (converter <- converters; number <- 1 to 3000) {
      number shouldBe converter.fromRomanNumerals(converter.toRomanNumerals(number))
    }
  }

}
