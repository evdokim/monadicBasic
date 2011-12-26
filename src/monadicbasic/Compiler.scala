//intermediate language

package monadicbasic

class Compiler {

    private var labelCounter = 0;

	def compile(statement: Statement):List[Op] = statement match {
    case LabelStatement(labelName) => List(LabelOp(labelName))
		case BlockStatement(statements) => statements.flatMap(compile(_)) 
		case PrintStatement(expr) => List(PrintOp(expr))
        case InputStatement(name) => List(InputOp(name))
        case GotoStatement(labelName) => List(JumpOp(labelName))
		case AssigmentStatement(name, expr) => List(AssigmentOp(name, expr))
    case AssigmentArrayStatement(name, index, expr) => 
        List(AssigmentArrayOp(name, index, expr))
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
              JumpIfOp(Greater(Var(name), toExpr), endLoopLabelName)
            ) ++
            compile(body) ++
            List(
              AssigmentOp(name, Add(Var(name), NumberLiteral(1))),
              JumpOp(beginLoopLabelName),
              LabelOp(endLoopLabelName)
            )
		}
        case IfStatement(codition:Expression, body:Statement) => {
            val endIfLabelName = "#ifEnd_" + labelCounter
            labelCounter += 1
            List(JumpIfNotOp(codition, endIfLabelName)) ++
            compile(body) :+
            LabelOp(endIfLabelName)
        }
	}
}
