package fpinscala.parsing

import scala.language.higherKinds
import scala.language.implicitConversions
import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

//trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait
trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Ex 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List()))

  def maybe[A](p: Parser[A]): Parser[Option[A]] =
    or(map(p)(Some(_)), succeed(None))

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a){x => succeed(f(x))}

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  // Ex 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case i => map2(p, listOfN(i - 1, p))(_ :: _)
  }

  // Ex 9.7
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap{a => p2.flatMap{b => succeed((a,b))}}

  // Ex 9.1
  def map2m[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)
  // Ex 9.7
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    (p ** p2).flatMap { ab => succeed(f.tupled(ab)) }
    //p.flatMap{a => p2.flatMap{b => succeed(f(a,b))}}

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // Ex 9.6
  def csParser: Parser[Int] = for {
    digit <- "[0-9]+".r
    n = digit.toInt
    _ <- listOfN(n, char('a'))
  } yield n
    //regex("[0-9]+".r).flatMap{n => listOfN(n.toInt, string("a")) }

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def maybe: Parser[Option[A]] = self.maybe(p)
    def slice: Parser[String] = self.slice(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))
    // Ex 9.2
    // tests associativity
    def productLaw[A,B](p: Parser[A], p2: Parser[B], p3: Parser[B])(in: Gen[String], in2: Gen[String]): Prop =
      forAll(in)(s => (p ** p2) ** p3 == p ** (p2 ** p3))
  }

  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import JSON._
    val spaces = char(' ').many.slice
    val digits = regex("[0-9]+".r)
    val number = for {
      negative <- char('-').maybe
      whole <- (char('0').map(_.toString) | (regex("[1-9]".r) ** digits).map{case (a, b) => a + b})
      decimal <- (char('.') ** digits).map(_._2).maybe
      expon <- (for {
        _ <- char('e') | char('E')
        sign <- (char('+') | char('-')).maybe
        d <- digits
      } yield {"E" + sign.getOrElse("+") + d}).maybe
    } yield {
      val s = (negative.getOrElse("") + whole + decimal.getOrElse("") + expon)
      JNumber(s.toDouble)
    }
    ???
  }
}



case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
