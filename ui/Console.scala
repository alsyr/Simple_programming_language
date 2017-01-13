package ui

import value._
import expression._

object console {

	val parsers = new EwokParsers // for now
			val globalEnv = new Environment

			def execute(cmmd: String): String = {
			val tree = parsers.parseAll(parsers.expression, cmmd)
					tree match {
					case t: parsers.Failure => throw new SyntaxException(t)
					case _ => "" + tree.get.execute(globalEnv)
			}
	}

	def repl {
		var more = true
				while(more) {
					try {
						// read/execute/print
						print("-> ")
						val cmmd = readLine();
						if(cmmd.equals("quit")){
							println("bye")
							more = false
						}
						else println(execute(cmmd))
					} 
					catch {
					case e: SyntaxException => {
						println(e.msg)
						println(e.result.msg)
						println("line # = " + e.result.next.pos.line)
						println("column # = " + e.result.next.pos.column)
						println("token = " + e.result.next.first)
					}
					case e: UndefinedException => {
						println(e.msg)
					}
          case e: TypeException => {
            println(e.msg)
          }
          case e: JediException => {
            println(e.msg)
          }
					} finally {
						//            Console.flush 
					}
				}
	}

	def main(args: Array[String]): Unit = {repl}

}