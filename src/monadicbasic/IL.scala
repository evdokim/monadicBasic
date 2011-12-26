//intermediate language

package monadicbasic

sealed abstract class Op
  case class ArrayDefOp(varName:String, typeName:String, size:Expression) extends Op
  case class VarDefOp(varName:String, typeName:String) extends Op
  case class PrintOp(expr:Expression) extends Op
  case class InputOp(name:String) extends Op
  case class AssigmentOp(name:String, expr:Expression) extends Op
  case class AssigmentArrayOp(name:String, index:Expression, 
	                          expr:Expression) extends Op
  case class JumpOp(target:String) extends Op
  case class JumpIfOp(pred:Expression, target:String) extends Op
  case class JumpIfNotOp(pred:Expression, target:String) extends Op
  case class LabelOp(name:String) extends Op
