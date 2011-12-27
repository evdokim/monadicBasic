//intermediate language

package monadicbasic

object Translator {

  private var labelCounter = 0;

  def translate(statement: Statement):List[Op] = statement match {

    case LabelStatement(labelName) => List(LabelOp(labelName))
    case BlockStatement(statements) => statements.flatMap(translate(_)) 
    case PrintStatement(expr) => List(PrintOp(expr))
    case InputStatement(name, pos) => List(InputOp(name, pos))
    case GotoStatement(labelName, pos) => List(JumpOp(labelName, pos))

    case AssigmentStatement(name, indexOpt, expr, pos) => indexOpt match {
      case Some(index) => List(AssigmentArrayOp(name, index, expr, pos))
      case None => List(AssigmentOp(name, expr, pos))
    }
            
    case DefStatement(varName, typeName, sizeExprOpt, pos) => sizeExprOpt match {
      case Some(sizeExpr) => List(ArrayDefOp(varName, typeName, sizeExpr, pos))
      case None => List(VarDefOp(varName, typeName, pos))
    }

    case ForStatement (body, name, (fromExpr, toExpr), pos) => {  
                
      val beginLoopLabelName = "#loopBegin_" + labelCounter
       val endLoopLabelName = "#loopEnd_" + labelCounter
       labelCounter += 1
       List(
         AssigmentOp(name, fromExpr, pos),
         LabelOp(beginLoopLabelName),
         JumpIfOp(Greater(Var(name, None, pos), toExpr, pos), endLoopLabelName, pos)
       ) ++
       translate(body) ++
       List(
         AssigmentOp(name, Add(Var(name, None, pos), NumberLiteral(1), pos), pos),
         JumpOp(beginLoopLabelName, pos),
         LabelOp(endLoopLabelName)
       )
    }
        case IfStatement(codition, body, pos) => {
            val endIfLabelName = "#ifEnd_" + labelCounter
            labelCounter += 1
            List(JumpIfNotOp(codition, endIfLabelName, pos)) ++
            translate(body) :+
            LabelOp(endIfLabelName)
        }
  }
}
