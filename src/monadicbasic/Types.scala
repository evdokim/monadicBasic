package monadicbasic

sealed abstract class Value {
	def typeName:String
}

case class ArrayValue (value: Array[Value], typename:String) extends Value {
	override def toString = "[" + value.foldLeft("")({(a,b) => a + " " + b}) + " ]"
    override def typeName = "array[" + typename + "]"
}

case class IntValue (value:Int) extends Value {
	override def toString = value.toString
	override def typeName = "integer"
} 

case class StringValue (value:String) extends Value {
	override def toString = value
	override def typeName = "string"
} 

case class BooleanValue (value:Boolean) extends Value {
	override def toString = value.toString
	override def typeName = "boolean"
}

case class Error(message:String) extends Value {
	override def typeName = "error"
}


object Types {
	def getDefaultValue(typeName: String, pos:Pos):Value = typeName match {
		case "integer" => IntValue(0)
		case "string" => StringValue("")
		case "boolean" => BooleanValue(true)
		case s => Error("type error: " + "at " + pos + " unsupported type: " + s)
	}

	def getDefaultArrayValue(typeName:String, size:Int, pos:Pos) =
		ArrayValue(
			new Array[Value](size).map({_ => getDefaultValue(typeName, pos)}),
			typeName
		)
}
