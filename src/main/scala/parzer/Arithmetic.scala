package parzer

/**
 * A parser for basic arithmetic using + * and ( )
 */
object Arithmetic {

  import CharParsers._

  def expr: Parser[Int] = {
    def tpluse(t: Int) = for {
      _ <- char('+')
      e <- expr
    } yield (t + e)

    for {t <- term
         t2 <- tpluse(t) or Parser(t)
    } yield t2
  }

  def term: Parser[Int] = {
    def exx(f: Int) = for {
      _ <- char('*')
      t <- term
    } yield (f * t)


    for {f <- factor
         f2 <- exx(f) or Parser(f)
    } yield f2
  }

  def factor: Parser[Int] = {
    (for {
      d <- digit
    } yield d.toString.toInt) or (for {
      _ <- char('(')
      e <- expr
      _ <- char(')')
    } yield e)
  }

  def eval(s: String): Option[Int] = {
    expr.parse(s.toList).map { case (i, s) => i }
  }
}
