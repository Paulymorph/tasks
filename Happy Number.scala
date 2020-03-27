object Solution {
  def isHappy(n: Int): Boolean = {
    def isHappyRecursive(slow: Int, fast: Int): Boolean = {
      if (fast == 1) true
      else if (slow == fast) false
      else isHappyRecursive(next(slow), next(next(fast)))
    }

    isHappyRecursive(n, next(n))
  }
  
  def next(n: Int): Int = {
    def squareDigitsRecursive(n: Int, acc: Int): Int = 
      if (n > 0) {
        val lastDigit = n % 10
        squareDigitsRecursive(n / 10, lastDigit * lastDigit + acc)
      } else acc

    squareDigitsRecursive(n, 0)
  }
}