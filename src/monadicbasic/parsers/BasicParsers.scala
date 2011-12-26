package monadicbasic.parsers

import monadicbasic._


object BasicParsers {
  def satisfy (pred:Char => Boolean, expected: String) = new Parser[Char] {
    def apply (in:List[Char]) = 
      in match {
        case Nil => new Failure("expected: " + expected + " eof found")
        case head :: tail => 
          if (pred(head)) new Success(head, tail) 
          else new Failure("expected: " + expected + " found: " + head)
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

  def label = digit.repPos.map(CharUtils.charsToString(_))


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


  def statement:Parser[Statement] = defStatement | forStatement | ifStatement | 
                                    printStatement | assignStatement | 
                                    assignArrayStatement | inputStatement |
                                    gotoStatement | labelStatement

  def defStatement = for {
            _ <- literal("DIM")
         name <- id
    arraySize <- between.opt
            _ <- literal("AS")
     typeName <- literal("String") | literal("Boolean") | literal("Integer")
  } yield DefStatement(name, typeName, arraySize)

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
    l <- label
  } yield GotoStatement(l)

  def assignStatement = for {
                         name <- id
                            _ <- literal("=")
                            e <- expr
                       } yield AssigmentStatement(name, e)
  
    def assignArrayStatement = for {
                         name <- id
                        index <- between
                            _ <- literal("=")
                            e <- expr
                       } yield AssigmentArrayStatement(name, index, e)
  //TODO next!!!
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
            _ <- id.opt
  } yield ForStatement(body, name, (from, to))
  

  def ifStatement = for {
       _ <- literal("IF")
    cond <- expr
       _ <- literal("THEN")
    stmt <- ifBlock | statement   
  } yield IfStatement(cond, stmt)

  def labelStatement = for {
    labelName <- label
  } yield LabelStatement(labelName)

  def ifBlock = for {
       _ <- endOfStatement
    body <- blockStatement
       _ <- literal("END IF")
  } yield body

  def expr:Parser[Expression] = expr4

  //expr 4
  def expr4 = less | greater | le | ge | eq | ne | expr3

  def less:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal("<")
    e2 <- expr4
  } yield Less(e1, e2)

  def le:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal("<=")
    e2 <- expr4
  } yield LE(e1, e2)

  def greater:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal(">")
    e2 <- expr4
  } yield Greater(e1, e2)

  def ge:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal(">=")
    e2 <- expr4
  } yield GE(e1, e2)


  def eq:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal("=")
    e2 <- expr4
  } yield Equal(e1, e2)

  def ne:Parser[Expression] = for {
    e1 <- expr3
     _ <- literal("<>")
    e2 <- expr4
  } yield NotEqual(e1, e2)

//p3
  def expr3 = add | sub | expr2

  def add:Parser[Expression] = for {
              e1 <- expr2
              _  <- literal("+")
              e2 <- expr3  
            } yield Add(e1, e2)
 
  def sub:Parser[Expression] = for {
              e1 <- expr2
              _  <- literal("-")
              e2 <- expr3  
            } yield Sub(e1, e2)
//p2  
  def expr2 = mult | div | expr1

  def mult:Parser[Expression] = for {
               e1 <- expr1
                _ <- literal("*")
               e2 <- expr2  
             } yield Mult(e1, e2)

  def div:Parser[Expression] = for {
               e1 <- expr1
                _ <- literal("/")
               e2 <- expr2  
             } yield Div(e1, e2)
//p1
  def expr1 = neg | expr0


  def neg = for {
               _ <- literal("-")
               e <- expr0  
             } yield Neg(e)

//zero level
  def expr0 = between | numberLiteral | stringLiteral | booleanLiteral | 
                  arrayVariable | variable

  def between = for {
                  _ <- literal("(")
                  e <- expr
                  _ <- literal(")")
                } yield e

  def digit = satisfy(CharUtils.isDigit(_), "digit")
    
  def numberLiteral = for {
              digits <- digit.repPos
            } yield NumberLiteral(CharUtils.charsToString(digits).toInt)
  
  def booleanLiteral = (literal("true") | literal("false")).map({
    v => BooleanLiteral(v.toBoolean)
  })
  def stringLiteral = for {
        _ <- char('"')
    chars <- satisfy({_ != '"'}, "not \"").rep
        _ <- char('"')
  } yield StringLiteral(CharUtils.charsToString(chars))

  def letter = satisfy(CharUtils.isLetter(_), "letter")
  
  
  def variable = for {
                   name <-id
                 } yield Var(name)
  
  def arrayVariable  = for {
     name <- id
    index <- between
  } yield ArrayVar(name, index)
  
}
