package monadicbasic

sealed abstract class Value

case class IntValue (value:Int) extends Value 
case class StringValue (value:String) extends Value
case class BooleanValue (value:Boolean) extends Value
case class Error(msg:String) extends Value

