
package object parzer {

  type PString = List[Char]

  /**
   * A parser can be flatmapped and can be added to another parser
   */
  class Parser[A](parser: PString => Option[(A, PString)]) {

    def parse(s: List[Char]): Option[(A, PString)] = parser(s)

    def parse(s: String): Option[(A, PString)] = parser(s.toList)

    /** flatmappable */
    //implement flatmap so that we can use the for notation to create bigger parsers out of parser parts
    def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    def map[B](f: A => B): Parser[B] = flatMap(a => Parser(f(a)))


    /** addable */
    def plus(q: Parser[A]): Parser[A] = new Parser(inp => parse(inp) match {
      case None => q.parse(inp)
      case Some((v, out)) => Some(v, out)
    })

    /** Choice : either parser p or parser q by 'adding' them together */
    def +++(q: Parser[A]) = plus(q)
  }

  object Parser {
    val empty = List[Char]()

    def apply[A](a: A) = new Parser(inp => Some(a, inp))

    def zero[A] = new Parser[A](inp => None)

    /** basic parsers */
    def failure[A]: Parser[A] = zero[A]

    def item: Parser[Char] = new Parser(inp => inp match {
      case List() => None
      case x :: xs => Some((x, xs))
    }
    )

    /** derived primitives */
    def satisfies(p: Char => Boolean): Parser[Char] = item.flatMap(x =>
      if (p(x)) Parser(x) else failure
    )

    def digit: Parser[Char] = satisfies(_.isDigit)

    def lower: Parser[Char] = satisfies(_.isLower)

    def upper: Parser[Char] = satisfies(_.isUpper)

    def letter: Parser[Char] = satisfies(_.isLetter)

    def alphanum: Parser[Char] = satisfies(_.isLetterOrDigit)

    def char(c: Char) = satisfies(_ == c)


    def string(s: PString): Parser[PString] = s match {
      case List() => Parser(empty)
      case x :: xs => for {_ <- char(x)
                           _ <- string(xs)
      } yield (x :: xs)
    }

    def string(s: String): Parser[PString] = string(s.toList)

    /** zero or more */
    def many[A](p: Parser[A]): Parser[List[A]] = many1(p) +++ Parser(List())

    /** one or more */
    def many1[A](p: Parser[A]): Parser[List[A]] = {
      for {v <- p
           vs <- many(p)
      } yield v :: vs
    }

    def ident: Parser[PString] = {
      for {x <- lower
           xs <- many(alphanum)
      } yield x :: xs
    }

    def nat: Parser[Int] = for {xs <- many1(digit)} yield xs.mkString.toInt

    //Define a parser that parses an integer literal.
    // An integer literal consists of an optional minus sign, followed by a sequence of one or more digits.
    def int: Parser[Int] = ???

    def space: Parser[Unit] = for {_ <- many(satisfies(_.isSpaceChar))} yield ()

    //Define a parser comment :: Parser () for ordinary Haskell-like comments that begin with the symbol //
    // and extend to the end of the current line, which is represented by the control character '\n' (beware Windows users!).
    def comment: Parser[Unit] = ???

    /*  Consider expressions built up from non-negative numbers, greater or equal to zero using a
    subtraction operator that associates to the left.

A possible grammar for such expressions would look as follows:
  expr ::= expr - nat | nat
  nat ::= 0 | 1 | 2 ...

However, this grammar is left-recursive and hence directly transliterating this grammar into parser combinators would
result in a program that does not terminate because of the left-recursion.
You can factor recursive grammars using iteration.

Choose an iterative implementation of left-asociative expressions that does not suffer from non-termination. */
    def expr: Parser[Int] = ???

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

  }

  object Parsing {

    import Parser._

    val p = for {x <- item
                 _ <- item
                 y <- item} yield (x, y)

    val q: Parser[PString] = for {
      _ <- char('[')
      d <- digit
      ds <- many(for {_ <- char(',')
                      d <- digit
      } yield d)
      _ <- char(']')
    } yield (d :: ds)

  }

  def main(a: Array[String]) = {

  }

}
