package parzer

/**
 * Parsers that ignore whitespace.
 */
object TokenParsers{
  import StringParsers._
  import Parser._

  // ignoring spacing
  def token[A](p: Parser[A]): Parser[A] = {
    for {_ <- space
         v <- p
         _ <- space
    } yield v
  }

  def identifier: Parser[PString] = token(ident)

  def natural: Parser[Int] = token(nat)

  def integer: Parser[Int] = token(int)

  def symbol(xs: PString): Parser[PString] = token(string(xs))

  def symbol(xs: String): Parser[PString] = symbol(xs.toList)

  /*  Consider expressions built up from non-negative numbers, greater or equal to zero using a
  subtraction operator that associates to the left.

A possible grammar for such expressions would look as follows:
expr ::= expr - nat | nat
nat ::= 0 | 1 | 2 ...

However, this grammar is left-recursive and hence directly transliterating this grammar into parser combinators would
result in a program that does not terminate because of the left-recursion.
You can factor recursive grammars using iteration.

Choose an iterative implementation of left-asociative expressions that does not suffer from non-termination. */
  def expr: Parser[Int] = {
    for {n <- natural
         ns <- many(for {_ <- symbol("-")
                         n1 <- natural} yield n1)
    } yield ns.foldLeft(n) { case (x, y) => x - y }
  }
}

