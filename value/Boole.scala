package value

import expression.Literal

case class Boole(value: Boolean) extends Literal {
  override def toString = value.toString
}