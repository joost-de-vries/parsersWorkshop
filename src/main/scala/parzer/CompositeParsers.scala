package parzer

/**
 * A few parsers that combine basic parsers.
 */
object CompositeParsers {

  import Parser._
  import CharParsers._

  val p = for {x <- item
               _ <- item
               y <- item} yield (x, y)

  val q: Parser[List[Int]] = for {
    _ <- char('[')
    d <- digit
    ds <- many(for {_ <- char(',')
                    d2 <- digit
    } yield d2.toString.toInt)
    _ <- char(']')
  } yield (d.toString.toInt :: ds)

}

