package expression

import value._

trait Expression {
  def execute(env: Environment): Value
}