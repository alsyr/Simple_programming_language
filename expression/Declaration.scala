package expression

import value._

case class Declaration(id: Identifier, init: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    // add a new row to env
    val theValue = init.execute(env)

    env += (id -> theValue.asInstanceOf[Value])
    Notification.DONE
  }
}