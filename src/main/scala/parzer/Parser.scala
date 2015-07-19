package object parzer {

  type PString = List[Char]

  /**
   * A parser can be flatmapped and can be added to another parser
   */
  class Parser[A](parser: PString => Option[(A, PString)]) {

    def parse(s: List[Char]): Option[(A, PString)] = parser(s)

    def parse(s: String): Option[(A, PString)] = parser(s.toList)

    /** flatmappable */
    def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser(inp => parser(inp) match {
      case None => None
      case Some((a, s)) => f(a).parse(s)
    })

    def map[B](f: A => B): Parser[B] = flatMap(a => Parser(f(a)))

    /** addable */
    def plus(q: Parser[A]): Parser[A] = new Parser(inp => parse(inp) match {
      case None => q.parse(inp)
      case Some((v, out)) => Some(v, out)
    })

    /** Choice : either parser p or parser q by 'adding' them together */
    def or(q: Parser[A]) = plus(q)
  }

  object Parser {

    def apply[A](a: A) = new Parser(inp => Some(a, inp))

    def zero[A] = new Parser[A](inp => None)

    /** zero or more */
    def many[A](p: Parser[A]): Parser[List[A]] = many1(p) or Parser(List())

    /** one or more */
    def many1[A](p: Parser[A]): Parser[List[A]] = {
      for {v <- p
           vs <- many(p)
      } yield v :: vs
    }
    def failure[A]: Parser[A] = zero[A]
  }
}
