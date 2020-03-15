// https://leetcode.com/problems/regular-expression-matching/
import scala.annotation.tailrec
object Solution {
  def isMatch(s: String, p: String): Boolean = {
      matches(s.toList, p.toList) match {
        case Matches => true
        case MatchNotFound(_) => false
      }
  }

  sealed trait Result
  case object Matches extends Result
  case class MatchNotFound(isFinal: Boolean) extends Result

  def matches(
    s: List[Char], 
    p: List[Char],
    debug: String => Unit = _ => ()
  ): Result = {
    debug(s"${s.mkString}; ${p.mkString}")
    (p, s) match {
      case ('*' :: '*' :: pTail, _) => matches(s, '*' :: pTail, debug)
      case ('.' :: '*' :: pTail, _) =>
        def findMatch(s: List[Char]): Option[Result] = {
          val tryFindMatch = matches(s, pTail, debug) 
          tryFindMatch match {
            case Matches | MatchNotFound(true) => Some(tryFindMatch)
            case _ => None
          }
        }

        findFirstForSublists(s, findMatch)
      
      case (symbol :: '*' :: pTail, _) =>
        def findMatch(s: List[Char]): Option[Result] = {
          val tryFindMatch = matches(s, pTail, debug) 
          tryFindMatch match {
            case Matches | MatchNotFound(true) => Some((tryFindMatch))
            case _ => s match {
              case `symbol` :: _ => None
              case _ => Some(tryFindMatch)
            }
          }
        }

        findFirstForSublists(s, findMatch)

      case ('.' :: pTail, _ :: sTail) => matches(sTail, pTail, debug)

      case (symbol :: pTail, same :: sTail) if symbol == same => 
        matches(sTail, pTail, debug)
      case (Nil, Nil) => Matches
      case (_,  another :: sTail) => 
        MatchNotFound(isFinal = false) // If we found a non match, then we should return to the previous star (if any) and try another string suffix
      case (any :: pTail, Nil) => MatchNotFound(isFinal = true)
    }
  }

  @tailrec
  def findFirstForSublists[A, Res](l: List[A], f: List[A] => Option[Res]): Res =
    f(l) match {
      case Some(res) => res
      case None => findFirstForSublists(l.tail, f)
    }
}

object Main extends App {
  val inputs = List(
    ("aa", "a", false),
    ("aab", "c*a*b", true),
    ("ho", "ho*", true),
    ("aa", ".*", true),
    ("cb", ".a", false),
    ("adceb", ".*a.*b", true),
    ("acdcb", "a.*c.b", false),
    ("aa", ".*a", true),
    ("aaa", ".*aa", true),
    ("aaaa", ".*.*.*a", true),
    ("", ".*", true),
    // ("babbbbaabababaabbababaababaabbaabababbaaababbababaaaaaabbabaaaabababbabbababbbaaaababbbabbbbbbbbbbaabbb", "b.*.*bb.*.*a.*.*bba.*b.*.*a.*bbb.*.*aba.*.*.*babbb.*aa.*.*.*.*aabb.*bbb.*.*.*a", false),
    // ("abbabaaabbabbaababbabbbbbabbbabbbabaaaaababababbbabababaabbababaabbbbbbaaaabababbbaabbbbaabbbbababababbaabbaababaabbbababababbbbaaabbbbbabaaaabbababbbbaababaabbababbbbbababbbabaaaaaaaabbbbbaabaaababaaaabb",".*.*aa.*.*.*.*.*ba.*a.*bb.*.*aa.*ab.*.*.*.*a.*aaaaaa.*.*.*a.*aaaa.*.*bbabb.*b.*b.*.*aaaaaaaaa.*a.*.*.*.*.*.*.*.*ba.*bbb.*.*.*a.*ba.*bb.*bb.*.*a.*b.*bb", false),
    ("mississippi", "mis*is*p*.", false),
    ("abefcdgiescdfimde", "ab.*cd.i.*de", true),
    // ("a"*1024, ".*" + "a"*1024 + ".*", true),
    // ("a"*1024, ".*" + "a"*1025 + ".*", false),
    // ("a"*1024, s".*${"a"*512}a.*${"a"*512}.*", false),
    // ("a"*1024, ".*"*1024 + "b", false)
  )

  inputs.foreach {
    case (s, p, expectation) =>
      val res = Solution.isMatch(s, p)
      if (res == expectation) {
        println(s"OK s = $s p=$p")
      } else {
        println(s"Wrong result on s = $s p=$p result $res")
        Solution.matches(s.toList, p.toList, println)
      }
  }
}