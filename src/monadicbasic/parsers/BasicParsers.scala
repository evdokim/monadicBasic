package monadicbasic.parsers

import monadicbasic.CharUtils

sealed abstract class Expression
  case class Add(e1:Expression, e2:Expression) extends Expression
  case class Mult(e1:Expression, e2:Expression) extends Expression
  case class NumberLiteral(value:Int) extends Expression

object BasicParsers {
  def satisfy (pred:Char => Boolean, exp: String) = new Parser[Char] {
    def apply (in:List[Char]) = 
      in match {
        case Nil => new Failure("expected: " + exp + " eof found")
        case head :: tail => 
          if (pred(head)) new Success(head, tail) 
          else new Failure("expected: " + exp + " found: " + head)
      }
  }

  def char (x:Char) = satisfy({_ == x}, String.valueOf(x))

  def literal (list:List[Char]):Parser[List[Char]] =
    list match {
      case Nil => Parser.unit(Nil)
      case head :: tail => 
        for {
          _ <- char(head)
          _ <- literal(tail)
        } yield list
    }

  def literal(s:String):Parser[String] = literal(s.toList).map(CharUtils.charsToString(_))

  def expr:Parser[Expression] = add | term

  def add = for {
              e1 <- term
              _  <- literal("+")
              e2 <- term  
            } yield new Add(e1, e2) 

  def term = mult | factor

  def mult = for {
               e1 <- factor
                _ <- literal("*")
               e2 <- factor  
             } yield new Mult(e1, e2)

  def factor = between | number

  def between = for {
                  _ <- literal("(")
                  e <- expr
                  _ <- literal(")")
                } yield e

  def digit = satisfy(CharUtils.isDigit(_), "digit")
    
  def number = for {
              digits <- digit.rep
            } yield new NumberLiteral(CharUtils.charsToString(digits).toInt)
}
