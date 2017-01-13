package expression

import value._
import ui._

// traverse elems, execute each one, if one isn't a Boole, throw an exception, if one returns false, return false immediately
case class Conjunction(elems: List[Expression]) extends SpecialForm {
	def execute(env: Environment) = {
		var result = true
				var i = 0
				while (result && i<elems.length){
					val v = elems(i).execute(env)
					if(!v.isInstanceOf[Boole]) throw new TypeException("conjunction input must be Booles")
          i = i+1
					result = v.asInstanceOf[Boole].value
				}
		new Boole(result)
	}
}
