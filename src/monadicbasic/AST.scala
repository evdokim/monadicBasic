package monadicbasic

sealed abstract class Expression
  case class Less(e1:Expression, e2:Expression) extends Expression
  case class LE(e1:Expression, e2:Expression) extends Expression
  case class Greater(e1:Expression, e2:Expression) extends Expression
  case class GE(e1:Expression, e2:Expression) extends Expression
  case class Equal(e1:Expression, e2:Expression) extends Expression
  case class NotEqual(e1:Expression, e2:Expression) extends Expression
  case class Add(e1:Expression, e2:Expression) extends Expression
  case class Sub(e1:Expression, e2:Expression) extends Expression
  case class Neg(e:Expression) extends Expression
  case class Mult(e1:Expression, e2:Expression) extends Expression
  case class Div(e1:Expression, e2:Expression) extends Expression
  case class Var(name:String) extends Expression
  case class NumberLiteral(value:Int) extends Expression

sealed abstract class Statement
  case class LabeledStatement(name: String, statement: Statement) extends Statement
  case class BlockStatement(list:List[Statement]) extends Statement
  case class PrintStatement(e:Expression) extends Statement
  case class InputStatement(name: String) extends Statement
  case class AssigmentStatement(name:String, e:Expression) extends Statement
  case class ForStatement (body: Statement, name:String, fromExpr: Expression, toExpr:Expression) extends Statement
  case class IfStatement (codition:Expression, body:Statement) extends Statement
  case class GotoStatement (labelName: String) extends Statement

