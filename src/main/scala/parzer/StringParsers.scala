package parzer

/**
 * Parser that interpret lists of characters. That is; strings.
 */
object StringParsers {

  import CharParsers._
  import Parser._

  def string(s: PString): Parser[PString] = s match {
    case List() => Parser(List())
    case x :: xs => for {_ <- char(x)
                         _ <- string(xs)
    } yield x :: xs
  }

  def string(s: String): Parser[PString] = string(s.toList)


  def ident: Parser[PString] = {
    for {x <- lower
         xs <- many(alphanum)
    } yield x :: xs
  }

  def nat: Parser[Int] = for {xs <- many1(digit)} yield xs.mkString.toInt

  //Define a parser that parses an integer literal.
  // An integer literal consists of an optional minus sign, followed by a sequence of one or more digits.
  def int: Parser[Int] = {
    val minP = for {_ <- char('-')
                    n <- nat} yield -n

    minP or nat
  }

  def space: Parser[Unit] = for {_ <- many(satisfies(_.isSpaceChar))} yield ()

  //Define a parser comment :: Parser () for ordinary Scala like comments that begin with the symbol //
  // and extend to the end of the current line, which is represented by the control character '\n' (beware Windows users!).
  def comment: Parser[Unit] = {
    for {_ <- string("//")
         _ <- many(satisfies(_ != '\n'))
    } yield ()
  }

}

