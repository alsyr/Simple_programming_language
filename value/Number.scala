package value

import expression.Literal

case class Number(value: Double) extends Literal {
  override def toString = value.toString
}