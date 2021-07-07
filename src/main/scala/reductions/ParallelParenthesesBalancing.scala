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
    val until = chars.length
    var idx = 0
    var nStart = 0
    var nEnd = 0
    while idx < until do
      chars(idx) match
        case '(' => nStart += 1
        case ')' => nEnd += 1
        case _ => ()
      if nEnd > nStart then
        nStart = 0
        nEnd = 1
        idx = until
      idx += 1

    nStart - nEnd == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int,Int) /*: ???*/ =
      var index = idx
      var breaks =0
      var end =0
      while index < until do
        chars(index) match
          case '(' => end += 1
          case ')' => end -= 1
          case _   => ()
        breaks = breaks.max(end)
        index += 1
      (breaks,end)

    def reduce(from: Int, until: Int): (Int, Int) /*: ???*/ =
      if until - from < threshold then
        traverse(from, until, 0, 0)
      else
        val mid = from + (until - from) / 2
        val (a1, a2) = parallel(reduce(from, mid), reduce(mid, until))
        ((a1._1 + a1._2) + a2._1, (a1._1 + a1._2) + a2._2)

    reduce(0, chars.length) == (0, 0)

// For those who want more:
// Prove that your reduction operator is associative!

