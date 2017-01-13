package value

import expression.Identifier
import scala.collection.mutable.HashMap

class Environment extends HashMap[Identifier, Value] with Value