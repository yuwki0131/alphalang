package example

import scala.util.parsing.combinator._

// TODO: Add if
// TODO: Add env/update/let-in
// TODO: Add Lambda/Closure
// Goal of Turing complete
// TODO: 文字列の実装

// AlphaLang Concrete Syntax Tree
trait ACST {}

trait Primitive extends ACST {}

case class Form(operator: ACST, operands: List[ACST]) extends ACST {
  override def toString(): String = {
    "Form(" + operator.toString + operands.map(op => " " + op.toString).toString + ")"
  }
}

case class Symbol(name: String) extends ACST {
  override def toString(): String = "symbol:" + name
}

case class LiteralInt(value: Int) extends Primitive {
  override def toString(): String = "literal:" + value.toString
}

case class LiteralBoolean(value: Boolean) extends Primitive {
  override def toString(): String = "literal:" + value.toString
}

// case class Literal(value: Bool) extends ACST {
//   override def toString(): String = "literal:" + value.toString
// }

case class Closure(symbols: List[Symbol], body: ACST, env:List[(Symbol, ACST)]) extends ACST {
  override def toString(): String = {
    "Closure(args = " + symbols.map(op => " " + op.toString).toString + ")"
  }
}

trait PrimitiveInt extends Primitive {
  def calcInts(operands:List[LiteralInt]):ACST
}

trait PrimitiveBoolean extends Primitive {
  def calcBoolean(operands:List[LiteralBoolean]):LiteralBoolean
}

case class PrimitiveNot() extends PrimitiveBoolean {
  override def toString(): String = "Primitive:not"

  override def calcBoolean(operands:List[LiteralBoolean]):LiteralBoolean = {
    operands.head match {
      case LiteralBoolean(value) => LiteralBoolean(! value)
      case _ => throw new RuntimeException
    }
  }
}

case class PrimitiveAnd() extends PrimitiveBoolean {
  override def toString(): String = "Primitive:and"

  override def calcBoolean(operands:List[LiteralBoolean]):LiteralBoolean = {
    LiteralBoolean(operands.map({case LiteralBoolean(value) => value}).reduceLeft({(a, b) => a && b}))
  }
}

case class PrimitiveOr() extends PrimitiveBoolean {
  override def toString(): String = "Primitive:or"

  override def calcBoolean(operands:List[LiteralBoolean]):LiteralBoolean = {
    LiteralBoolean(operands.map({case LiteralBoolean(value) => value}).reduceLeft({(a, b) => a || b}))
  }
}

case class PrimitiveLessThan() extends PrimitiveInt {
  override def toString(): String = "Primitive:<"

  def repeatGt(result:Boolean, operands:List[Int]):Boolean = {
    if ((! result) || (operands.tail.isEmpty)){
      result
    } else {
      repeatGt(operands(0) < operands(1), operands.tail)
    }
  }

  override def calcInts(operands:List[LiteralInt]):LiteralBoolean = {
    LiteralBoolean(repeatGt(true, operands.map({case LiteralInt(value) => value})))
  }
}

case class PrimitiveEquals() extends PrimitiveInt {
  override def toString(): String = "Primitive:="

  def repeatEq(result:Boolean, operands:List[Int]):Boolean = {
    if ((! result) || (operands.tail.isEmpty)){
      result
    } else {
      repeatEq(operands(0) == operands(1), operands.tail)
    }
  }

  override def calcInts(operands:List[LiteralInt]):LiteralBoolean = {
    LiteralBoolean(repeatEq(true, operands.map({case LiteralInt(value) => value})))
  }
}

case class PrimitivePlus() extends PrimitiveInt {
  override def toString(): String = "Primitive:+"

  override def calcInts(operands:List[LiteralInt]):LiteralInt = {
    LiteralInt(operands.map({case LiteralInt(value) => value}).reduceLeft({(a, b) => a + b}))
  }
}

case class PrimitiveMinus() extends PrimitiveInt {
  override def toString(): String = "Primitive:-"

  override def calcInts(operands:List[LiteralInt]):LiteralInt = {
    LiteralInt(operands.map({case LiteralInt(value) => value}).reduceLeft({(a, b) => a - b}))
  }
}

case class PrimitiveMult() extends PrimitiveInt {
  override def toString(): String = "Primitive:*"

  override def calcInts(operands:List[LiteralInt]):LiteralInt = {
    LiteralInt(operands.map({case LiteralInt(value) => value}).reduceLeft({(a, b) => a * b}))
  }
}

case class PrimitiveDiv() extends PrimitiveInt {
  override def toString(): String = "Primitive:/"

  override def calcInts(operands:List[LiteralInt]):LiteralInt = {
    LiteralInt(operands.map({case LiteralInt(value) => value}).reduceLeft({(a, b) => a / b}))
  }
}

case class PrimitiveMod() extends PrimitiveInt {
  override def toString(): String = "Primitive:%"

  override def calcInts(operands:List[LiteralInt]):LiteralInt = {
    LiteralInt(operands.map({case LiteralInt(value) => value}).reduceLeft({(a, b) => a % b}))
  }
}

/** Simple Parser of AlphaLang */
class AlphaParser extends RegexParsers {

  def literalInt: Parser[ACST]    = """\-?[0-9]+""".r ^^ { lit => LiteralInt(lit.toInt) }

  def literalBoolean: Parser[ACST]= """(true)|(false)""".r ^^ { lit => LiteralBoolean(lit.toBoolean) }

  def symbol: Parser[ACST]     = (
    """[a-zA-Z\/\!\$\%\&\=\-\~\~\|\+\*\:\?\<\>][0-9a-zA-Z\!\$\%\&\=\-\~\~\|\+\*\:\?\<\>]*""".r
      ^^ { symbol => Symbol(symbol) }
  )

  def form: Parser[ACST] = (
    (("(" ~> (expression ~ (expression*))) <~ ")" )
      ^^ { case operator ~ operands => Form(operator, operands) }
  )

  def expression: Parser[ACST] = literalInt | literalBoolean | symbol | form

  def sourcecode: Parser[List[ACST]] = (expression *) <~ """$""".r
}

object AlphaEval {

  /** binding primitive functions & variables here */
  val initEnv = List(
    (Symbol("+"), PrimitivePlus()),
    (Symbol("-"), PrimitiveMinus()),
    (Symbol("*"), PrimitiveMult()),
    (Symbol("/"), PrimitiveDiv()),
    (Symbol("%"), PrimitiveMod()),
    (Symbol("<"), PrimitiveLessThan()),
    (Symbol("="), PrimitiveEquals()),
    (Symbol("not"), PrimitiveNot()),
    (Symbol("and"), PrimitiveAnd()),
    (Symbol("or"), PrimitiveOr())
  )

  /** find variable of environment */
  def find(symbol: Symbol, env:List[(Symbol, ACST)]): ACST = env match {
    case (symbolInEnv, exp)::restEnv if symbolInEnv.equals(symbol) => exp
    case (symbolInEnv, exp)::restEnv => find(symbol, restEnv)
    case _ => throw new RuntimeException
  }

  /** binding variable */
  def update(binds: List[ACST], env:List[(Symbol, ACST)]):List[(Symbol, ACST)] = {
    if (binds.isEmpty){
      env
    } else {
      binds match {
        case Form(symbol @ Symbol(_), body: List[ACST])::(rest :List[ACST]) => {
          update(rest, (symbol, evalSExp(body.head, env))::env)
        }
        case _ => throw new RuntimeException
      }
    }
  }

  // TODO
  def update(symbols: List[Symbol], values: List[ACST], env:List[(Symbol, ACST)]):List[(Symbol, ACST)] = {
    List()
  }

  /** evaluate s-expression */
  def evalSExp(expression: ACST, env:List[(Symbol, ACST)]):ACST = expression match {
    case Form(Symbol("if"), List(condtion, thenClause, elseClause)) => { // if expression
      if (evalSExp(condtion, env).equals(LiteralInt(1))) (evalSExp(thenClause, env))
      else (evalSExp(elseClause, env))
    }
    case Form(Symbol("lambda"), List(symbols:Form, body)) => { // generate closure (function def)
      val symbolsList = ((symbols.operator.asInstanceOf[Symbol])::symbols.operands.asInstanceOf[List[Symbol]]): List[Symbol]
      Closure(symbolsList, body, env)
    }
    case Form(Symbol("let"), List(binds:Form, body)) => { // binding variable
      val letBinds = (binds.operator::binds.operands): List[ACST]
      evalSExp(body, update(letBinds, env))
    }
    case Form(operator, operands) => { // apply function
      val args = operands.map(arg => evalSExp(arg, env))
      val f = evalSExp(operator, env)
      f match {
        case primitiveInt: PrimitiveInt => primitiveInt.calcInts(args.asInstanceOf[List[LiteralInt]])
        case primitiveBoolean: PrimitiveBoolean => primitiveBoolean.calcBoolean(args.asInstanceOf[List[LiteralBoolean]])
        case Closure(symbols, body, env) => evalSExp(body, update(symbols, args, env))
      }
    }
    case symbol:Symbol => find(symbol, env)
    case primitive:Primitive => primitive
    case _ => {
      throw new RuntimeException
    }
  }
}

object AlphaLang extends AlphaParser {

  def main(args: Array[String]) = {
    println("source code:" + args(0))
    parse(sourcecode, args(0)) match {
      case Success(matched, _) => {
        println("parsed tree:" + matched.head.toString)
        println(AlphaEval.evalSExp(matched.head, AlphaEval.initEnv).toString)
      }
      case Failure(msg, _) => println("Parse Failure: " + msg)
      case Error(msg, _) => println("ERROR: "+ msg)
     }
  }
}

// sbt run "(+ 1 2)"
