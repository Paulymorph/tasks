// https://leetcode.com/problems/word-search-ii/
object Solution {
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val trie = words.foldLeft(Trie.empty[Char])(_ ++ _.toList)
    val boardContext = new Board(board)
      import boardContext._
      val allCoordinates = for {
        x <- board.indices
        y <- board(x).indices
        coordinate <- Coordinate(x, y)
      } yield coordinate

    allCoordinates.toList.flatMap { start =>
      trie.edges.get(start.letter).fold(List.empty[String]) {
        nextTrie => solve(start, Set.empty, nextTrie, List.empty)
      }
    }.distinct
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

    def solve(
        currentStep: Coordinate, 
        visited: Set[Coordinate], 
        trieLeft: Trie[Char],
        collectedWord: List[Char]
      ): List[String] = {
      val nextSteps = currentStep.adjacent -- visited
      val nextCollectedWord = currentStep.letter :: collectedWord
      val foundWords = if (trieLeft.isFinish) 
          List(nextCollectedWord.reverse.mkString)
        else List.empty
      nextSteps.foldLeft(foundWords) {
        case (foundSoFar, nextStep) =>
          val nextTrieOpt = trieLeft.edges.get(nextStep.letter)
          nextTrieOpt.fold(foundSoFar) { nextTrie =>
            foundSoFar ++ solve(nextStep, visited + currentStep, nextTrie, nextCollectedWord)
          }
      }
    }
  }

  sealed case class Trie[T](edges: Map[T, Trie[T]], isFinish: Boolean = false) {
    final def ++(word: List[T]): Trie[T] = 
      word match {
        case letter :: tail => 
          val furtherTrie = edges.get(letter).getOrElse(Trie.empty)
          val newFurtherTrie = furtherTrie ++ tail
          new Trie(edges + (letter -> newFurtherTrie), isFinish)
        case Nil => new Trie(edges, isFinish = true)
      }
    
    final def contains(word: List[T]): Boolean = 
      word match {
        case letter :: tail => edges.get(letter).fold(false)(_.contains(tail))
        case Nil => isFinish
      }
    
    final def containsPrefix(prefix: List[T]): Boolean = 
      prefix match {
        case letter :: tail => edges.get(letter).fold(false)(_.containsPrefix(tail))
        case Nil => true
      }
  }

  object Trie {
    def apply[T](word: List[T]): Trie[T] = {
      empty[T] ++ word
    }

    def empty[T]: Trie[T] = new Trie[T](Map.empty) {}
  }
}

object Main extends App {
  val board = Array(
    Array('o','a','a','n'),
    Array('e','t','a','e'),
    Array('i','h','k','r'),
    Array('i','f','l','v')
  )

  val words = Array("oath","pea","eat","rain")
  println(Solution.findWords(board, words))
}