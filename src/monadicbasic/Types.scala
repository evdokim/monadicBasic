package monadicbasic

sealed abstract class Value {
	def typeName:String
}

case class ArrayValue (value: Array[Value], typename:String) extends Value {
	override def toString = "[" + value.foldLeft("")({(a,b) => a + " " + b}) + " ]"
    override def typeName = "Array[" + typename + "]"
}

case class IntValue (value:Int) extends Value {
	override def toString = value.toString
	override def typeName = "Integer"
} 

case class StringValue (value:String) extends Value {
	override def toString = value
	override def typeName = "String"
} 

case class BooleanValue (value:Boolean) extends Value {
	override def toString = value.toString
	override def typeName = "Boolean"
}

case class Error(message:String) extends Value {
	override def typeName = "Error"
}




object Types {
	def getDefaultValue(typeName: String):Value = typeName match {
		case "Integer" => IntValue(0)
		case "String" => StringValue("")
		case "Boolean" => BooleanValue(true)
		case s => Error("unsupported type: " + s)
	}

	def getDefaultArrayValue(typeName:String, size:Int) =
		ArrayValue(
			new Array[Value](size).map({_ => getDefaultValue(typeName)}),
			typeName
		)

	/*def getTypeName(v:Value):String = v match {
		case ArrayValue(value, type) => "Array[" + type + "]"
		case IntValue(value) => "Integer"
		case StringValue(value) => "String"
		case BooleanValue(value) => "Boolean"

	}*/
}
