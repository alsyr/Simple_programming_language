package expression

import value._
import ui._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  
  def execute(env: Environment) = {
    
    val args: List[Value] = operands.map(_.execute(env)).map(_.asInstanceOf[Value]) // eagerly execute operands
   
    if (env.contains(operator)) {
      // apply operator to args starting in Wookie
      null
    } else {
      system.execute(operator.value, args)
    }  
  }
}