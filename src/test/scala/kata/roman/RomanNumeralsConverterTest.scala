package kata.roman

import org.specs2.execute.Result
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

/**
 * Unit test using specs2
 */
class RomanNumeralsConverterTest extends Specification {

  /**
   * converter implementation to test
   */
  private val converter: RomanNumeralsConverter = new RomanNumeralsConverterImpl

  /**
   * valid numbers are in the range 1-3000,
   * some example invalid numbers
   */
  private val invalidNumbers = List(-1, 0, 3001)

  /**
   * map of example valid integers to roman numerals
   */
  private val validNumberRomanNumerals = Map(
    1745 -> "MDCCXLV", 33 -> "XXXIII", 45 -> "XLV",
    1999 -> "MCMXCIX", 3000 -> "MMM", 1888 -> "MDCCCLXXXVIII")

  "The RomanNumeralsConverter.toRomanNumerals" >> {
    "throws IllegalArgumentException for invalid numbers: "+invalidNumbers >> {
      Result.foreach(invalidNumbers) { number =>
        converter.toRomanNumerals(number) must
          throwA[IllegalArgumentException](s"number: $number is not in range 1-3000")
      }
    }
    Fragment.foreach(validNumberRomanNumerals.toSeq) { kv =>
      val (k,v) = kv
      s"converts number: $k to roman numerals: $v" ! {converter.toRomanNumerals(k) mustEqual v}
    }
  }

  "The RomanNumeralsConverter.fromRomanNumerals" >> {
    val nullEmptyMessage = "romanNumerals String is null or empty"
    s"throws IllegalArgumentException '$nullEmptyMessage' for null" >> {
      converter.fromRomanNumerals(null) must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    s"throws IllegalArgumentException '$nullEmptyMessage' for empty String" >> {
      converter.fromRomanNumerals("") must throwA[IllegalArgumentException](nullEmptyMessage)
    }
    val invalidRomanNumerals = List("A", "B", "Z", "BII")
    "throws IllegalArgumentException for invalid roman numerals: "+invalidRomanNumerals >> {
      Result.foreach(invalidRomanNumerals) { romanNumeral =>
        converter.fromRomanNumerals(romanNumeral) must
          throwA[IllegalArgumentException](s"$romanNumeral contains illegal characters")
      }
    }
    Fragment.foreach(validNumberRomanNumerals.toSeq) { kv =>
      val (k,v) = kv
      s"converts roman numerals: $v to number: $k" ! {converter.fromRomanNumerals(v) mustEqual k}
    }
  }

  "The RomanNumeralsConverter.toRomanNumerals & then fromRomanNumerals" >> {
    "results in the original number for numbers 1 - 3000" >> {
      Result.foreach(1 to 3000) { number =>
        number mustEqual converter.fromRomanNumerals(converter.toRomanNumerals(number))
      }
    }
  }

}
