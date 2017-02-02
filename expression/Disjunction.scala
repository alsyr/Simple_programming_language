package expression

import value._
import ui._

// traverse elems, execute each one, if one isn't a Boole, throw an exception, if one returns
// true, return true immediately
case class Disjunction(elems: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var result = false
    var i = 0
    while (!result && i < elems.length) {
      val v = elems(i).execute(env)
      if (!v.isInstanceOf[Boole]) throw new TypeException("disjunction input must be Booles")
      i = i + 1
      result = v.asInstanceOf[Boole].value
    }
    new Boole(result)
  }
}