package monadicbasic.parsers
import monadicbasic._

object BasicParsers {

  def id = new Parser[String] {
    val keyWords = Set("for", "to", "next", "input", "print", "if", "end", "then", "while", 
                       "dim", "as", "integer", "string", "boolean")
    def apply (in:List[(Token, Pos)]) = in match {
        case Nil => Failure("expected: id eof found")
        case (head @ (WordToken(s), _)) :: tail if keyWords.contains(s) => 
                     Failure("at line: " + head._2.line + " column: " + head._2.column +
                     " " + s + " is key word") 
        case (head @ (WordToken(s), _)) :: tail => Success(s, tail) 
        case head :: tail => Failure("at line: " + head._2.line + " column: " + head._2.column +
                       " expected: id found: " + head._1)
      }
  }

  def string(s:String) = new Parser[String] {
    def apply (in:List[(Token, Pos)]) = in match {
        case Nil => Failure("expected: " + s + " eof found")
        case (head @ (WordToken(w), _)) :: tail if w==s => Success(w, tail) 
        case head :: tail => Failure("at line: " + head._2.line + " column: " + head._2.column +
                       " expected: " + s + " found: " + head._1)
      }
  }
  
  def number = new Parser[Int] {
    def apply (in:List[(Token, Pos)]) = in match {
        case Nil => Failure("expected: number eof found")
        case (head @ (NumberToken(n), _)) :: tail => Success(n, tail) 
        case head :: tail => Failure("at line: " + head._2.line + " column: " + head._2.column +
                       " expected: number found: " + head._1)
      }
  }

  def newline = new Parser[Unit] {
    def apply (in:List[(Token, Pos)]) = in match {
        case Nil => Failure("expected: \\n eof found")
        case (NewLineToken, _) :: tail => Success(None, tail) 
        case head :: tail => Failure("at line: " + head._2.line + " column: " + head._2.column +
                       " expected: \\n found: " + head._1)
      }
  }

 //priority 0 

  def numberLiteral = number.map({n => NumberLiteral(n)}) 

  def stringLiteral = new Parser[StringLiteral] {
    def apply (in:List[(Token, Pos)]) = in match {
        case Nil => Failure("expected: string literal eof found")
        case (head @ (StringLiteralToken(s), _)) :: tail => Success(StringLiteral(s), tail) 
        case head :: tail => Failure("at line: " + head._2.line + " column: " + head._2.column +
                       " expected: string literal found: " + head._1)
      }
  } 

  def between:Parser[Expression] = for {
    _ <- string("(")
    e <- expr
    _ <- string(")")
  } yield e

  def varriable = for {
    name <- id
       e <- between.opt
  } yield Var(name, e)
  
  def booleanLiteral = (string("true") || string("false")).map({
    s => BooleanLiteral(s.toBoolean)
  })


  def expr0:Parser[Expression] = between || numberLiteral || stringLiteral || 
                                 booleanLiteral || varriable

  //priority 1
  def expr1 = neg || expr0

  def neg = for {
    _ <- string("-")
    e <- expr0  
  } yield Neg(e)

  //priority 2 
  def expr2 = mult | div | expr1

  def mult:Parser[Expression] = for {
    e1 <- expr1
     _ <- string("*")
    e2 <- expr2  
  } yield Mult(e1, e2)

  def div:Parser[Expression] = for {
    e1 <- expr1
     _ <- string("/")
    e2 <- expr2  
  } yield Div(e1, e2)

  //priority 3
  def expr3 = add | sub | expr2

  def add:Parser[Expression] = for {
    e1 <- expr2
     _ <- string("+")
    e2 <- expr3  
  } yield Add(e1, e2)
 
  def sub:Parser[Expression] = for {
    e1 <- expr2
    _  <- string("-")
    e2 <- expr3  
  } yield Sub(e1, e2)


  //priority 4
  def expr4 = less | greater | le | ge | eq | ne | expr3

  def less:Parser[Expression] = for {
    e1 <- expr3
     _ <- string("<")
    e2 <- expr4
  } yield Less(e1, e2)

  def le:Parser[Expression] = for {
    e1 <- expr3
     _ <- string("<=")
    e2 <- expr4
  } yield LE(e1, e2)

  def greater:Parser[Expression] = for {
    e1 <- expr3
     _ <- string(">")
    e2 <- expr4
  } yield Greater(e1, e2)

  def ge:Parser[Expression] = for {
    e1 <- expr3
     _ <- string(">=")
    e2 <- expr4
  } yield GE(e1, e2)

  def eq:Parser[Expression] = for {
    e1 <- expr3
     _ <- string("=")
    e2 <- expr4
  } yield Equal(e1, e2)

  def ne:Parser[Expression] = for {
    e1 <- expr3
     _ <- string("<>")
    e2 <- expr4
  } yield NotEqual(e1, e2)

  def expr:Parser[Expression] = expr4

  //statements
  def statement:Parser[Statement] = defStatement || forStatement || ifStatement || 
                                    printStatement || assignStatement || inputStatement ||
                                    gotoStatement || labelStatement
  
  def defStatement = for {
            _ <- string("dim")
         name <- id
    arraySize <- between.opt
            _ <- string("as")
     typeName <- string("string") | string("boolean") || string("integer")
  } yield DefStatement(name, typeName, arraySize)

  def printStatement = for {
    _ <- string("print")
    e <- expr
  } yield PrintStatement(e)

  def inputStatement = for {
       _ <- string("input")
    name <- id
  } yield InputStatement(name)                     

  def gotoStatement = for {
    _ <- string("goto")
    n <- number
  } yield GotoStatement(n.toString)

  def assignStatement = for {
     name <- id
    index <- between.opt
        _ <- string("=")
        e <- expr
  } yield AssigmentStatement(name, index, e)
  
  def forStatement = for {
       _ <- string("for")
    name <- id
       _ <- string("=")
    from <- expr
       _ <- string("to") 
      to <- expr
       _ <- endOfStatement
    body <- blockStatement
       _ <- string("next") 
  } yield ForStatement(body, name, (from, to))
  

  def ifStatement = for {
       _ <- string("if")
    cond <- expr
       _ <- string("then")
    stmt <- ifBlock || statement   
  } yield IfStatement(cond, stmt)

  def ifBlock = for {
       _ <- endOfStatement
    body <- blockStatement
       _ <- string("end")
       _ <- string("if")
  } yield body

  def labelStatement = for {
    labelName <- number
  } yield LabelStatement(labelName.toString)

  def closedStatement = for {
    st <- statement
     _ <- endOfStatement.repPos
  } yield st

  
  def blockStatement = for {
             _ <- endOfStatement.rep
    statements <- closedStatement.rep
  } yield BlockStatement(statements)


  def endOfStatement = newline || string(":")

}
