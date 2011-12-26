package monadicbasic.parsers

sealed abstract class ParseResult[+T] {
  def map[U] (f: T=>U) = 
    this match {
      case Success(pRes, pRem) =>  new Success(f(pRes), pRem)
      case Failure(s) => new Failure(s)
    }
}

case class Success[T](result: T, next: List[Char]) extends ParseResult[T]
case class Failure(msg: String) extends ParseResult[Nothing]

trait Parser[+T] extends (List[Char]=>ParseResult[T]) { p =>
  def flatMap[U] (f: T=>Parser[U]) = new Parser[U] {
    def apply (in:List[Char]) =
      p(in) match {
        case Success(pRes, pRem) =>
          f(pRes) apply pRem match {
            case Success(qRes, qRem) => new Success(qRes, qRem)
            case Failure(s) => new Failure(s)
          }
        case Failure(s) => new Failure(s)
      }
  }

  def map[U] (f: T=>U) = new Parser[U] {
    def apply(in: List[Char]) =
      p(in) match {
        case Success(pRes, pRem) =>  new Success(f(pRes), pRem)
        case Failure(s) => new Failure(s)
      }
  }

  def |[U >: T] (q: => Parser[U]) = new Parser[U] {
    def apply(in: List[Char]) =
      p(in) match {
        case Success(pRes, pRem) =>  new Success(pRes, pRem)          
        case Failure(s) => 
          q(in) match {
            case Success(qRes, qRem) =>  new Success(qRes, qRem)
            case Failure(s) => new Failure(s)
          }
      }
  }

  def ~[U] (q: => Parser[U]): Parser[(T, U)] =
    for {
      a <- p
      b <- q
    } yield (a,b)
  

  //TODO companion object Parser ????
  def repPos:Parser[List[T]] = (for {head <- p; tail <- p.rep} yield head :: tail) | p.map(List(_))    
  
  def rep:Parser[List[T]] = (for {head <- p; tail <- p.rep} yield head :: tail) | Parser.unit(List()) 
  
  def opt:Parser[Option[T]] = p.map(Some(_)) | Parser.unit(None)
  
}

object Parser {
  def unit [T] (t: T) = new Parser[T] {
    def apply(in: List[Char]) = new Success(t, in)    
  } 
}
