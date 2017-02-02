package ui

import value._

object system {

  // the dispatcher
  def execute(operator: String, args: List[Value]) = {
    operator match {
      case "add" => add(args)
      case "product" => product(args)
      case "div" => div(args)
      case "equals" => equal(args)
      case "less" => less(args)
      case "great" => great(args)
      case "not" => not(args)
      case _ => throw new UndefinedException(operator)
    }
  }

  def add(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("addition needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("addition inputs must be numbers")

    new Number(args.map(_.asInstanceOf[Number]).map(_.value).reduce(_ + _))
  }

  def product(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("product needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("product inputs must be numbers")

    new Number(args.map(_.asInstanceOf[Number]).map(_.value).reduce(_ * _))
  }

  def div(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("division needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("division inputs must be numbers")

    new Number(args.map(_.asInstanceOf[Number]).map(_.value).reduce(_ / _))
  }

  def equal(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("equals needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("equals inputs must be numbers")

    var isEqual = true
    val argsCheck = args.map(_.asInstanceOf[Number])
    for (i <- 1 until argsCheck.length)
      if (argsCheck(0).value != argsCheck(i).value) isEqual = false

    new Boole(isEqual)
  }

  def less(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("less needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("less inputs must be numbers")

    var isLess = true
    val argsCheck = args.map(_.asInstanceOf[Number])
    for (i <- 1 until argsCheck.length)
      if (argsCheck(i).value <= argsCheck(i - 1).value) isLess = false

    new Boole(isLess)
  }

  def great(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("great needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("great inputs must be numbers")

    var isGreater = true
    val argsCheck = args.map(_.asInstanceOf[Number])
    for (i <- 1 until argsCheck.length)
      if (argsCheck(i).value >= argsCheck(i - 1).value) isGreater = false

    new Boole(isGreater)
  }

  def not(args: List[Value]) = {
    // type check like crazy first
    if (args.isEmpty) throw new TypeException("not needs some inputs")
    for (arg <- args)
      if (!arg.isInstanceOf[Number]) throw new TypeException("not inputs must be boolean")
    if (args.length > 1) throw new TypeException("too many arguments")

    val argsCheck = args.map(_.asInstanceOf[Boole])

    new Boole(!argsCheck(0).value)
  }
}