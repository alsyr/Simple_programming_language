package expression

import value._
import ui._

case class Identifier(value: String) extends Expression {
  def execute(env: Environment) =
    if (env.contains(this)) env(this) else throw new UndefinedException(value)
}