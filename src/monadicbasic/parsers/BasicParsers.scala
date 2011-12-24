package monadicbasic.parsers

import monadicbasic._


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

  def literalStrong (list:List[Char]):Parser[List[Char]] =
    list match {
      case Nil => Parser.unit(Nil)
      case head :: tail => 
        for {
          _ <- char(head)
          _ <- literalStrong(tail)
        } yield list
    }
  
  def literal  (list:List[Char]):Parser[List[Char]] = for {
    _ <- char(' ').rep
    l <- literalStrong(list)
    _ <- char(' ').rep
  } yield l

  def literal(s:String):Parser[String] = literal(s.toList).map(CharUtils.charsToString(_))
  //TODO: add optional to statement
  
  def id = for {
             r <- letter ~ (letter | digit).rep
           } yield CharUtils.charsToString(r._1 :: r._2)

  def label = (letter | digit).repPos.map(CharUtils.charsToString(_))


  def closedStatement = for {
    st <- statement
     _ <- endOfStatement.repPos
  } yield st

  
  def blockStatement = for {
             _ <- endOfStatement.rep
    statements <- closedStatement.rep
  } yield BlockStatement(statements)
  def endOfStatement = literal("\n") | literal(":")

  //def nopStatement = Parsers.unit(NopStatement)
  def statement = labeledStatement | unlabeledStatement
  
  def labeledStatement = for {
    labelName <- label
            _ <- literal(":")
         stat <- unlabeledStatement
  } yield LabeledStatement(labelName, stat)

  def unlabeledStatement:Parser[Statement] = forStatement | ifStatement | printStatement | assignStatement | inputStatement | gotoStatement

  def printStatement = for {
                         _ <- literal("PRINT")
                         e <- expr
                       } yield PrintStatement(e)

  def inputStatement = for {
       _ <- literal("INPUT")
    name <- id
  } yield InputStatement(name)                     

  def gotoStatement = for {
            _ <- literal("GOTO")
    labelName <- label
  } yield GotoStatement(labelName)

  def assignStatement = for {
                         name <- id
                            _ <- literal("=")
                            e <- expr
                       } yield AssigmentStatement(name, e)
  def forStatement = for {
       _ <- literal("FOR")
    name <- id
       _ <- literal("=")
    from <- expr
       _ <- literal("TO") 
      to <- expr
       _ <- endOfStatement
    body <- blockStatement
       _ <- literal("NEXT") 
    name2 <- id
  } yield if (name == name2) {
     new ForStatement(body, name, from, to)
  } else {
    //TODO
    new ForStatement(body, name, from, to)
  }

  def ifStatement = for {
       _ <- literal("IF")
    cond <- expr
       _ <- literal("THEN")
    stmt <- ifBlock | statement   
  } yield IfStatement(cond, stmt)

  def ifBlock = for {
       _ <- endOfStatement
    body <- blockStatement
       _ <- literal("END IF")
  } yield body

  def expr:Parser[Expression] = expr4

  //expr 4
  def expr4 = less | greater | le | ge | eq | ne | expr3

  def less = for {
    e1 <- expr3
     _ <- literal("<")
    e2 <- expr3
  } yield Less(e1, e2)

  def le = for {
    e1 <- expr3
     _ <- literal("<=")
    e2 <- expr3
  } yield LE(e1, e2)

  def greater = for {
    e1 <- expr3
     _ <- literal(">")
    e2 <- expr3
  } yield Greater(e1, e2)

  def ge = for {
    e1 <- expr3
     _ <- literal(">=")
    e2 <- expr3
  } yield GE(e1, e2)


  def eq = for {
    e1 <- expr3
     _ <- literal("=")
    e2 <- expr3
  } yield Equal(e1, e2)

  def ne = for {
    e1 <- expr3
     _ <- literal("<>")
    e2 <- expr3
  } yield NotEqual(e1, e2)

//p3
  def expr3 = add | sub | expr2

  def add = for {
              e1 <- expr2
              _  <- literal("+")
              e2 <- expr2  
            } yield Add(e1, e2)
 
  def sub = for {
              e1 <- expr2
              _  <- literal("-")
              e2 <- expr2  
            } yield Sub(e1, e2)
//p2  
  def expr2 = mult | div | expr1

  def mult = for {
               e1 <- expr1
                _ <- literal("*")
               e2 <- expr1  
             } yield Mult(e1, e2)

  def div = for {
               e1 <- expr1
                _ <- literal("/")
               e2 <- expr1  
             } yield Div(e1, e2)
//p1
  def expr1 = neg | expr0


  def neg = for {
               _ <- literal("-")
               e <- expr0  
             } yield Neg(e)

//zero level
  def expr0 = between | number | variable

  def between = for {
                  _ <- literal("(")
                  e <- expr
                  _ <- literal(")")
                } yield e

  def digit = satisfy(CharUtils.isDigit(_), "digit")
    
  def number = for {
              digits <- digit.repPos
            } yield NumberLiteral(CharUtils.charsToString(digits).toInt)

  def letter = satisfy(CharUtils.isLetter(_), "letter")
  
  
  def variable = for {
                   name <-id
                 } yield Var(name)
  
}
