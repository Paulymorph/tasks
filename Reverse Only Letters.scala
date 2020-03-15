// https://leetcode.com/problems/reverse-only-letters/

object Solution {
    def reverseOnlyLetters(s: String): String = {
      val lettersReversed = s.toStream.reverse.filter(_.isLetter)
      s.foldLeft((StringBuilder.newBuilder, lettersReversed)) {
        case ((res, nextReversedLetter #:: lettersReversedTail), letter) if letter.isLetter =>
          (res.append(nextReversedLetter), lettersReversedTail)
        case ((res, lettersReversed), symbol) =>
          (res.append(symbol), lettersReversed)
      }._1.mkString
    }
}

val inputs = List(
  "ab-cd" -> "dc-ba", 
  "a-bC-dEf-ghIj" -> "j-Ih-gfE-dCba",
  "Test1ng-Leet=code-Q!" -> "Qedo1ct-eeLg=ntse-T!"
)

inputs.foreach {
  case (input, expected) =>
    val res = Solution.reverseOnlyLetters(input)
    if (res == expected)
      println(s"OK $input")
    else println(s"ERROR on $input, expected $expected but result $res")
}