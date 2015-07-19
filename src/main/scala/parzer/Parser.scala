
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
    def or(q: Parser[A]) = plus(q)
  }

  object Parser {
    val empty = List[Char]()

    def apply[A](a: A) = new Parser(inp => Some(a, inp))

    def zero[A] = new Parser[A](inp => None)

    /** basic parsers */
    def failure[A]: Parser[A] = zero[A]

    /** zero or more */
    def many[A](p: Parser[A]): Parser[List[A]] = many1(p) or Parser(List())

    /** one or more */
    def many1[A](p: Parser[A]): Parser[List[A]] = {
      for {v <- p
           vs <- many(p)
      } yield v :: vs
    }
  }
}
