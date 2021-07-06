package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface :

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    @tailrec
    def insideMethodBalance(chars: Array[Char], isCorrectBalance: Boolean, n: Int): Boolean =
      if chars.isEmpty then isCorrectBalance
      else
        chars.head match
          case '(' => insideMethodBalance(chars.tail, false, n + 1)
          case ')' => isCorrectBalance match
            case true => false
            case false => n match
              case x if x == 1 => insideMethodBalance(chars.tail, true, n - 1)
              case x if x > 1 => insideMethodBalance(chars.tail, false, n - 1)
          case _ => insideMethodBalance(chars.tail, isCorrectBalance, n)

    if (chars.isEmpty) true
    else
      insideMethodBalance(chars, true, 0)

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): List[Int] /*: ???*/ = {
      var startParenthese = arg1
      var endParenthese = arg2
      val out = List[Int]()
      while idx < until do
        chars(idx) match {
          case '(' => out +: 1
          case ')' => out +: -1
        }
      out
    }

    def reduce(from: Int, until: Int): Int /*: ???*/ = {
      ???
    }

    reduce(0, chars.length) == 0

// For those who want more:
// Prove that your reduction operator is associative!

