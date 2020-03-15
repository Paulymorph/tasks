// https://leetcode.com/problems/wildcard-matching/

import scala.annotation.tailrec
object Solution {
  def isMatch(s: String, p: String): Boolean = {
      matches(s.toStream, p.toStream) match {
        case Matches => true
        case MatchNotFound(_) => false
      }
  }

  sealed trait Result
  case object Matches extends Result
  case class MatchNotFound(isFinal: Boolean) extends Result

  private val Nil = Stream.empty
  def matches(
    s: Stream[Char], 
    p: Stream[Char],
    debug: String => Unit = _ => ()
  ): Result = {
    debug(s"${s.mkString}; ${p.mkString}")
    (p, s) match {
      case ('?' #:: pTail, _ #:: sTail) => matches(sTail, pTail, debug)
      case ('*' #:: '*' #:: pTail, _) => matches(s, '*' #:: pTail, debug)
      case ('*' #:: Nil, _) => 
        Matches // star in the pattern end "eats" everything
      case ('*' #:: pTail, _) =>
        // * eats an empty symbol first
        // If a result is "matches" or result has flag of "final answer" return the result
        // If a solution not found, let the * eat one more symbol
        // Until the * eats the whole string (if so, return complete not found)
        def findMatch(s: Stream[Char]): Either[Continue.type, Result] = {
          val tryFindMatch = matches(s, pTail, debug) 
          tryFindMatch match {
            case Matches | MatchNotFound(true) => Right(tryFindMatch)
            case _ => Left(Continue)
          }
        }

        findFirstForSublists(s, findMatch, onEmpty = MatchNotFound(isFinal = true))
      case (symbol #:: pTail, same #:: sTail) if symbol == same => 
        matches(sTail, pTail, debug)
      case (Nil, Nil) => Matches
      case (_,  another #:: sTail) => 
        MatchNotFound(isFinal = false) // If we found a non match, then we should return to the previous star (if any) and try another string suffix
      case (any #:: pTail, Nil) => MatchNotFound(isFinal = true)
    }
  }

  case object Continue

  // A generic function for applying function f to list l and its sublists (detaching head)
  // until we find a result
  // onEmpty is returned when the incoming list is cut down to empty
  // f should return Right(result) when a solution is found and Left(continue) when the 
  // list should be cut down 
  @tailrec
  def findFirstForSublists[A, Res](l: Stream[A], f: Stream[A] => Either[Continue.type, Res], onEmpty: Res): Res =
    l match {
      case _ #:: tail => f(l) match {
        case Right(res) => res
        case Left(Continue) => findFirstForSublists(tail, f, onEmpty)
      }
      case Nil => onEmpty
    }
}