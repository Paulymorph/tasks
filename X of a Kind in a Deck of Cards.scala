// https://leetcode.com/problems/x-of-a-kind-in-a-deck-of-cards/

object Solution {
    def hasGroupsSizeX(deck: Array[Int]): Boolean = {
        val numbersCount = deck.foldLeft(Map.empty[Int, Int]) { case (count, i) =>
            val iCount = count.get(i).getOrElse(0) + 1
            count + (i -> iCount)
        }
        
        numbersCount.values.foldLeft(numbersCount.values.head)(gcd(_, _)) > 1
    }
    
    def gcd(a: Int, b: Int): Int = {
        if (b == 0) a
        else gcd(b, a % b)
    }
}