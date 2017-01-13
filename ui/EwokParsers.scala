package ui

import scala.util.parsing.combinator._
import expression._
import value._

class EwokParsers extends RegexParsers {
  
//  EXPRESSION ::= DECLARATION | CONDITIONAL | DISJUNCTION
//  DECLARATION ::= def~IDENTIFIER~=~EXPRESSION 
//  CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
//  DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*
//  
//  CONJUNCTION ::= EQUALITY~(&&~EQUALITY)*
//  EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
//  INEQUALITY ::= ADD~((<|>|<=|>=|!=)~ADD)*
//  ADD ::= PRODUCT~((\+|-)~PRODUCT)*
//  PRODUCT ::= FUNCALL~((\*|/)~FUNCALL)*
//  
//  FUNCALL ::= TERM~OPERANDS?
//  OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
//  
//  TERM ::= LITERAL | IDENTIFIER  | (~EXPRESSION~)
//  
//  LITERAL ::= BOOLE | NUMBER
//  
//  IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
//  BOOLE ::= true | false
//  NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  
  //  EXPRESSION ::= DECLARATION | CONDITIONAL | DISJUNCTION
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  //  DECLARATION ::= def~IDENTIFIER~=~EXPRESSION 
  def declaration: Parser[Declaration] = "def"~identifier~"="~expression ^^ {
       case "def"~id~"="~exp => Declaration(id, exp)
   }  
  
  //  CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
  def conditional: Parser[Conditional] = "if"~"("~expression~")"~expression~opt("else"~>expression) ^^ {
       case "if"~"("~condition~")"~consequent~None => Conditional(condition, consequent)
       case "if"~"("~condition~")"~consequent~Some(alternate) => Conditional(condition, consequent, alternate)
  }
  
  //  DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*  
  def disjunction: Parser[Expression] = conjunction ~ rep("||"~>conjunction) ^^{
      case conj~Nil => conj
      case conj~more => Disjunction(conj::more)
  }
   
  //  FUNCALL ::= TERM~OPERANDS?
  def funcall: Parser[Expression] = term ~ opt(operands) ^^
  {
    case t~None => t
    case t~Some(Nil) => FunCall(t.asInstanceOf[Identifier], Nil)
    case t~Some(ops) => FunCall(t.asInstanceOf[Identifier], ops)
  }
  
  //  OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep(","~>expression))<~")" ^^
  {
     case None => Nil 
     case Some(e ~ Nil) => List(e) 
     case Some(e ~ exps) => e::exps 
     case _ => Nil
  }
  
  //  TERM ::= LITERAL | IDENTIFIER  | (~EXPRESSION~)   
  def term: Parser[Expression] = literal | identifier | "("~>expression<~")" 
  
  //  LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Literal] = boole | numeral
  
  //  IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ 
  {
    case e => Identifier(e)
  }
  
  //  NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def numeral: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ 
  {
    case e => Number(e.toDouble)
  }
  
  //  BOOLE ::= true | false
  def boole: Parser[Boole] = """true|false""".r ^^ 
  {
    case e => Boole(e.toBoolean)
  }
  
  //  CONJUNCTION ::= EQUALITY~(&&~EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&"~>equality)^^{
      case eq~Nil=> eq
      case eq~more => Conjunction(eq::more)
  }
  
  //  EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("=="~>inequality)^^{
      case ineq~Nil => ineq
      case ineq~more => FunCall(Identifier("equals"), ineq::more)
  }
  
  //  INEQUALITY ::= ADD~((<|>|<=|>=|!=)~ADD)*
  def inequality: Parser[Expression] = add ~ rep("<"~>add)^^ {
      case i~Nil => i
      case i~rest =>FunCall(Identifier("less"), i::rest)
  }
  
  //  ADD ::= PRODUCT~((\+|-)~PRODUCT)*
  def add: Parser[Expression] = 
    product ~ rep(("+"|"-") ~ product ^^ {case "+"~s=>s case "-"~s=>negate(s)})^^{
      case p~Nil=> p
      case p~rest=>FunCall(Identifier("add"), p::rest)
  }
  
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }
    
  //  PRODUCT ::= FUNCALL~((*|/)~FUNCALL)*
  def product: Parser[Expression] = 
    funcall ~ rep(("*"|"/") ~ funcall^^{case ("*"|"/")~s=>s})^^{
      case f~Nil=> f
      case f~rest=>FunCall(Identifier("product"), f::rest)
  }

}