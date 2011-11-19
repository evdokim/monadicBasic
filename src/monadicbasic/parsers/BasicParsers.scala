package monadicbasic.parsers

import monadicbasic.CharUtils

sealed abstract class Expression {
  def eval(env: Map[String, Int]):Int
}

case class Add(e1:Expression, e2:Expression) extends Expression {
  override def eval(env: Map[String, Int]) = e1.eval(env) + e2.eval(env)
}

case class Sub(e1:Expression, e2:Expression) extends Expression {
  override def eval(env: Map[String, Int]) = e1.eval(env) - e2.eval(env)
}

case class Opp(e:Expression) extends Expression {
  override def eval(env: Map[String, Int]) = -e.eval(env)
}

case class Mult(e1:Expression, e2:Expression) extends Expression {
  override def eval(env: Map[String, Int]) = e1.eval(env) * e2.eval(env)
}

case class Div(e1:Expression, e2:Expression) extends Expression {
  override def eval(env: Map[String, Int]) = e1.eval(env) / e2.eval(env)
}

case class Var(name:String) extends Expression {
  override def eval(env: Map[String, Int]) = {println(name) ; env(name)}
}

case class NumberLiteral(value:Int) extends Expression {
  override def eval(env: Map[String, Int]) = value
}

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

  def expr:Parser[Expression] = add | sub | term

  def add = for {
              e1 <- term
              _  <- literal("+")
              e2 <- term  
            } yield new Add(e1, e2)
 
  def sub = for {
              e1 <- term
              _  <- literal("-")
              e2 <- term  
            } yield new Sub(e1, e2)
  
  def term = mult | div | factor

  def mult = for {
               e1 <- factor
                _ <- literal("*")
               e2 <- factor  
             } yield new Mult(e1, e2)

  def div = for {
               e1 <- factor
                _ <- literal("/")
               e2 <- factor  
             } yield new Div(e1, e2)

  def factor = oppValue | value

  def oppValue = for {
               _ <- literal("-")
               e <- value  
             } yield new Opp(e)

  def value = between | number | variable

  def between = for {
                  _ <- literal("(")
                  e <- expr
                  _ <- literal(")")
                } yield e

  def digit = satisfy(CharUtils.isDigit(_), "digit")
    
  def number = for {
              digits <- digit.repPos
            } yield new NumberLiteral(CharUtils.charsToString(digits).toInt)

  def letter = satisfy(CharUtils.isLetter(_), "letter")
  
  def id = for {
             r <- letter ~ (letter | digit).rep
           } yield CharUtils.charsToString(r._1 :: r._2)

  def variable = for {
                   name <-id
                 } yield new Var(name)
  
}
