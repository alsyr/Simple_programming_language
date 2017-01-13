package expression

import value._

trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}