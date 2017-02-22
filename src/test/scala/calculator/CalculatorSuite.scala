package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("ca1") {
    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      ("a" -> Var(Literal(2))),
      ("b" -> Var(Ref("a")))
    ))
    assert(values("b")() == 2)
  }

  test("ca2") {
    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      ("a" -> Var(Literal(2))),
      ("b" -> Var(Plus(Literal(1), Ref("a"))))
    ))
    assert(values("b")() == 3)
  }

  test("ca3") {
    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      ("a" -> Var(Literal(2))),
      ("b" -> Var(Plus(Literal(1), Ref("a")))),
      ("c" -> Var(Times(Ref("b"), Ref("a"))))
    ))
    assert(values("a")() == 2)
    assert(values("b")() == 3)
    assert(values("c")() == 6)
  }

  test("ca4") {
    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      ("d" -> Var(Plus(Ref("e"), Literal(1)))),
      ("e" -> Var(Plus(Ref("d"), Literal(1))))
    ))
    // d = e + 1
    // e = d + 1
    assert(values("d")().isNaN)
    assert(values("e")().isNaN)
  }

  test("ca5") {
    val values: Map[String, Signal[Double]] = Calculator.computeValues(Map(
      ("d" -> Var(Plus(Ref("e"), Literal(1)))),
      ("e" -> Var(Plus(Ref("f"), Literal(1)))),
      ("f" -> Var(Plus(Ref("e"), Literal(1))))
    ))
    assert(values("d")().isNaN)
    assert(values("e")().isNaN)
    assert(values("f")().isNaN)
  }

}
