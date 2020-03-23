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

        private val adjacentDeltas: Set[(Int, Int)] = {
          val oneDimensionDeltas = (-1 to 1).toSet
          for {
            deltaX <- oneDimensionDeltas
            deltaY <- oneDimensionDeltas
            if deltaX != 0 || deltaY != 0
          } yield (deltaX, deltaY)
        }
      }

      def solve(currentStep: Coordinate, visited: Set[Coordinate], wordLeft: List[Char]): Boolean = {
        wordLeft match {
          case doesntMatch :: _ if doesntMatch != currentStep.letter => false
          case matches :: wordLeft =>
            currentStep.adjacent.filterNot(visited.contains).exists { nextStep =>
              solve(nextStep, visited + nextStep, wordLeft)
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

  val maxN = 100
  val big = Array.fill(maxN)(Array.fill(maxN)('a'))
  check(big, "a"*999 + "b", false)
}