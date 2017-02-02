package ui

import scala.util.parsing.combinator._

class JediException(val msg: String = "Your force is weak") extends Exception(msg)

class TypeException(val tmsg: String = "Type error") extends JediException(tmsg)

class UndefinedException(val symbol: String)
    extends JediException("Undefined identifier: " + symbol)

class SyntaxException(val result: Parsers#Failure) extends JediException("Syntax error")