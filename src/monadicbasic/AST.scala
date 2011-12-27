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
  case class Var(name:String, index: Option[Expression]) extends Expression
  case class NumberLiteral(value:Int) extends Expression
  case class StringLiteral(value:String) extends Expression
  case class BooleanLiteral(value:Boolean) extends Expression

sealed abstract class Statement
  case class LabelStatement(labelName: String) extends Statement
  case class BlockStatement(list: List[Statement]) extends Statement
  case class DefStatement(varName: String, typeName: String, 
                          arraySize: Option[Expression]) extends Statement
  case class PrintStatement(e:Expression) extends Statement
  case class InputStatement(name: String) extends Statement
  case class AssigmentStatement(name:String, 
                        index: Option[Expression], e:Expression) extends Statement

  case class ForStatement (body: Statement, name: String, 
                           bounds: (Expression, Expression)) extends Statement
  case class IfStatement (codition:Expression, body:Statement) extends Statement
  case class GotoStatement (label: String) extends Statement

