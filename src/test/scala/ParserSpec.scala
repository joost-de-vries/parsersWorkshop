import org.scalatest._
import parzer.CompositeParsers

class ParserSpec extends FlatSpec with Matchers {
  import CompositeParsers._
  "parser example p" should "parse successfully if input matches definition" in {
    p.parse("abcdef") should be(Some(('a','c'),List('d','e','f')))
  }
  it should "fail as a whole if one parser fails" in {
    p.parse("ab") should be(None)
  }

  "parser example q" should "parse digits if brackets are matched" in {
    q.parse("[1,2,3,4]") should be(Some(List(1,2,3,4),List()))
  }
}
