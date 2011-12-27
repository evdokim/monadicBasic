package monadicbasic.parsers

import monadicbasic._

sealed abstract class ParseResult[+T] {
  def map[U] (f: T=>U) = 
    this match {
      case Success(res, rem) =>  Success(f(res), rem)
      case Failure(msg) => Failure(msg)
      case FatalFailure(msg) => Failure(msg)
    }
}

case class Success[T](result: T, next: List[(Token, Pos)]) extends ParseResult[T]
case class Failure(msg: String) extends ParseResult[Nothing]
case class FatalFailure(msg: String) extends ParseResult[Nothing]

trait Parser[+T] extends (List[(Token, Pos)]=>ParseResult[T]) { p =>
  def flatMap[U] (f: T=>Parser[U]) = new Parser[U] {
    def apply (in:List[(Token, Pos)]) = p(in) match {
        case Success(pRes, pRem) => f(pRes) apply pRem match {
            case success: Success[_] => success
            case Failure(s) => FatalFailure(s)
            case fatalFailure => fatalFailure
          }
        case failure:Failure => failure
        case fatalFailure:FatalFailure => fatalFailure
      }
  }

  def map[U] (f: T=>U) = new Parser[U] {
    def apply(in: List[(Token, Pos)]) = p(in) match {
      case Success(pRes, pRem) => Success(f(pRes), pRem)
      case failure:Failure => failure
      case fatalFailure:FatalFailure => fatalFailure
    }
  }

//with out bt
  def ||[U >: T] (q: => Parser[U]) = new Parser[U] {
    def apply(in: List[(Token, Pos)]) = p(in) match {
      case success: Success[_] => success          
      case Failure(s) => q(in)
      case fatalFailure => fatalFailure
    }
  }

  def |[U >: T] (q: => Parser[U]) = new Parser[U] {
    def apply(in: List[(Token, Pos)]) = p(in) match {
      case success: Success[_] => success          
      case Failure(s) => q(in)
      case fatalFailure => q(in)
    }
  }

  def ~[U] (q: => Parser[U]): Parser[(T, U)] = for {
    a <- p
    b <- q
  } yield (a,b)
  
  def repPos: Parser[List[T]] = for {
    head <- p
    tail <- p.rep
  } yield head :: tail

  def rep: Parser[List[T]] = repPos || Parser.unit(List())

  def opt:Parser[Option[T]] = p.map(Some(_)) | Parser.unit(None)
  
}


object Parser {
  def unit [T] (t: T) = new Parser[T] {
    def apply(in: List[(Token, Pos)]) = new Success(t, in)    
  } 
}
