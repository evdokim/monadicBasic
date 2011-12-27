package monadicbasic


sealed abstract class Value {
  def getType:TypeInfo
}

case class ArrayValue (value: Array[Value], typeInfo: TypeInfo) extends Value {
  override def toString = "[" + value.foldLeft("")({(a,b) => a + " " + b}) + " ]"
  override def getType = typeInfo
}

case class IntValue (value:Int) extends Value {
  override def toString = value.toString
  override def getType = IntegerType
} 

case class StringValue (value:String) extends Value {
  override def toString = value
  override def getType = StringType
}

case class BooleanValue (value:Boolean) extends Value {
  override def toString = value.toString
  override def getType = BooleanType
}

case class Error(message:String) extends Value {
  override def toString = message
  override def getType = ErrorType
}


sealed abstract class TypeInfo
  case object IntegerType extends TypeInfo
  case object BooleanType extends TypeInfo
  case object StringType extends TypeInfo
  case object ArrayType extends TypeInfo
  case object ErrorType extends TypeInfo

object Types {
  val byNameMap = Map(
    "integer" -> IntegerType,
    "boolean" -> BooleanType,
    "string" -> StringType,
    "array" -> ArrayType,
    "error" -> ErrorType
  )

  val defaultValues = Map[TypeInfo, Value](
    IntegerType -> IntValue(0),
    BooleanType -> BooleanValue(true),
    StringType -> StringValue(""),
    ArrayType -> Error("type error"),
    ErrorType -> Error("type error")
  )

  def getDefaultValue(typeInfo: TypeInfo): Value = defaultValues(typeInfo)

  def getDefaultArrayValue(typeInfo: TypeInfo, size: Int) =
    ArrayValue(
      new Array[Value](size).map({_ => getDefaultValue(typeInfo)}),
      typeInfo
    )
}
