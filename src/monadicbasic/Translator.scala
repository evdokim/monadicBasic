//intermediate language

package monadicbasic

object Translator {

  private var labelCounter = 0;

  def translate(statement: Statement):List[Op] = statement match {

    case LabelStatement(labelName) => List(LabelOp(labelName))
    case BlockStatement(statements) => statements.flatMap(translate(_)) 
    case PrintStatement(expr) => List(PrintOp(expr))
    case InputStatement(name) => List(InputOp(name))
    case GotoStatement(labelName) => List(JumpOp(labelName))

    case AssigmentStatement(name, indexOpt, expr) => indexOpt match {
      case Some(index) => List(AssigmentArrayOp(name, index, expr))
      case None => List(AssigmentOp(name, expr))
    }
            
    case DefStatement(varName, typeName, sizeExprOpt) => sizeExprOpt match {
      case Some(sizeExpr) => List(ArrayDefOp(varName, typeName, sizeExpr))
      case None => List(VarDefOp(varName, typeName))
    }

    case ForStatement (body, name, (fromExpr, toExpr)) => {  
                
      val beginLoopLabelName = "#loopBegin_" + labelCounter
            val endLoopLabelName = "#loopEnd_" + labelCounter
            labelCounter += 1
            List(
              AssigmentOp(name, fromExpr),
              LabelOp(beginLoopLabelName),
              JumpIfOp(Greater(Var(name, None), toExpr), endLoopLabelName)
            ) ++
            translate(body) ++
            List(
              AssigmentOp(name, Add(Var(name, None), NumberLiteral(1))),
              JumpOp(beginLoopLabelName),
              LabelOp(endLoopLabelName)
            )
    }
        case IfStatement(codition:Expression, body:Statement) => {
            val endIfLabelName = "#ifEnd_" + labelCounter
            labelCounter += 1
            List(JumpIfNotOp(codition, endIfLabelName)) ++
            translate(body) :+
            LabelOp(endIfLabelName)
        }
  }
}
