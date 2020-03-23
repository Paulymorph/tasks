// https://leetcode.com/problems/word-search/
object Solution {
    def exist(board: Array[Array[Char]], word: String): Boolean = {
      val boardContext = new Board(board)
      import boardContext._
      val allCoordinates = for {
        x <- board.indices
        y <- board(x).indices
        coordinate <- Coordinate(x, y)
      } yield coordinate

      allCoordinates.exists(solve(_, Set.empty, word.toList))
    }

    class Board(board: Array[Array[Char]]) {
      abstract case class Coordinate private(x: Int, y: Int) {
        def letter: Char = board(x)(y)
        def adjacent: Set[Coordinate] = {
          Coordinate.adjacentDeltas.flatMap { 
            case (deltaX, deltaY) => Coordinate(x + deltaX, y + deltaY)
          }
        }
      }

      object Coordinate {
        def apply(x: Int, y: Int): Option[Coordinate] = {
          if (0 <= x && x < board.length &&
              0 <= y && y < board(x).length)
                Some(new Coordinate(x, y){})
          else None
        }

        private val adjacentDeltas: Set[(Int, Int)] = Set(-1 -> 0, 0 -> -1, 1 -> 0, 0 -> 1)
      }

      def solve(currentStep: Coordinate, visited: Set[Coordinate], wordLeft: List[Char]): Boolean = {
        wordLeft match {
          case doesntMatch :: _ if doesntMatch != currentStep.letter => false
          case matches :: Nil => true
          case matches :: wordLeft =>
            val newVisited = visited + currentStep
            currentStep.adjacent.filterNot(visited.contains).exists { nextStep =>
              solve(nextStep, newVisited, wordLeft)
            }
          case Nil => true
        }
      }
    }
}

object Main extends App {
  val small = Array(
    Array('A','B','C','E'),
    Array('S','F','C','S'),
    Array('A','D','E','E')
  )

  def check(board: Array[Array[Char]], word: String, expected: Boolean): Unit = {
    val result = Solution.exist(board, word)
    if (result == expected)
      println(s"OK $word")
    else
      println(s"Wrong result $result on $word")
  }

  check(small, "ABCCED", true)
  check(small, "SEE", true)
  check(small, "ABCB", false)
  check(Array(Array('a')), "a", true)
  check(Array(Array.fill(2)('a')), "a"*3, false)

  val maxN = 100
  val big = Array.fill(maxN)(Array.fill(maxN)('a'))
  // check(big, "a"*999 + "b", false)
}