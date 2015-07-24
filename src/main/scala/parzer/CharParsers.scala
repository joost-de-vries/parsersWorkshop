package parzer

/**
 * Basic parsers that interpret individual characters.
 */
object CharParsers {
  import Parser._

  def item: Parser[Char] = new Parser({
    case List() => None
    case x :: xs => Some((x, xs))
  })

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
}

