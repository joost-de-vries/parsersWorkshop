import org.scalatest._

class ArithmeticSpec extends FlatSpec with Matchers {
  import parzer.Arithmetic._
  "Arithmetic eval" should "give * higher priority than +" in {
    eval("2*3+4") should === (Some(10))
  }
  it should "evaluate parenthesised expressions correctly" in {
    eval("2*(3+4)") should be (Some(14))
  }
}
