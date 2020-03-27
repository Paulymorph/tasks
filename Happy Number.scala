object Solution {
  def isHappy(n: Int): Boolean = {
    def isHappyRecursive(n: Int, visited: Set[Int]): Boolean = {
      if (visited.contains(n)) false
      else if (n == 1) true
      else isHappyRecursive(next(n), visited + n)
    }
    
    isHappyRecursive(n, Set.empty)
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