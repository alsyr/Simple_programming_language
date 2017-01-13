package expression

import value._
import ui._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
   def execute(env: Environment) = {
     condition.execute(env) match {
       case Boole(value) => 
         if (value) consequent.execute(env) 
         else if (alternative != null) alternative.execute(env) else Notification.UNSPECIFIED
       case _ => throw new TypeException("if condition must be Boole")
     }
   }
}