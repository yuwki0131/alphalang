package example

import scala.util.parsing.combinator._

// AlphaLang Concrete Syntax Tree
trait ACST {}

trait Primitive extends ACST {}

case class Symbol(name: String) extends ACST {
  override def toString(): String = "symbol:" + name
}

case class LiteralInt(value: Int) extends Primitive {
  override def toString(): String = "literal:" + value.toString
}

case class LiteralBoolean(value: Boolean) extends Primitive {
  override def toString(): String = "literal:" + value.toString
}

case class Form(operator: ACST, operands: List[ACST]) extends ACST {
  override def toString(): String = {
    "Form(" + operator.toString + operands.map(op => " " + op.toString).toString + ")"
  }
}

trait SpecialForm extends ACST {}

case class IfForm(condition: ACST, thenClause: ACST, elseClause: ACST) extends SpecialForm {
  override def toString(): String = {
    s"if ${condition} then ${thenClause} else ${elseClause}"
  }
}

case class LetForm(binding:List[BindingForm], body: ACST) extends SpecialForm {
  override def toString(): String = {
    s"let ${binding} in ${body}"
  }
}

case class BindingForm(symbol: Symbol, body: ACST) extends SpecialForm {
  override def toString(): String = {
    s"bind ${symbol} = ${body}, "
  }
}

case class LambdaForm(symbols:List[Symbol], body: ACST) extends SpecialForm {
  override def toString(): String = {
    s"(lambda ( ${symbols} ) ${body})"
  }
}

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

  def symbol: Parser[Symbol]      = (
    """[a-zA-Z\/\!\$\%\&\=\-\~\~\|\+\*\:\?\<\>][0-9a-zA-Z\!\$\%\&\=\-\~\~\|\+\*\:\?\<\>]*""".r
      ^^ { symbol => Symbol(symbol) }
  )

  def form: Parser[ACST] = (
    (("(" ~> (expression ~ (expression*))) <~ ")" )
      ^^ { case operator ~ operands => Form(operator, operands) }
  )

  def ifForm: Parser[ACST] = (
    (("(" ~> ("if" ~> (expression ~ expression ~ expression))) <~ ")" )
      ^^ { case cond ~ thenClause ~ elseClause => IfForm(cond, thenClause, elseClause) }
  )

  def bindingForm: Parser[BindingForm] = (
    (("(" ~> (symbol ~ expression)) <~ ")" )
      ^^ { case symbol ~ body => BindingForm(symbol, body) }
  )

  def letForm: Parser[ACST] = (
    (("(" ~> ("let" ~> ((("(" ~> rep(bindingForm)) <~ ")") ~ expression))) <~ ")" )
      ^^ { case binds ~ body => LetForm(binds, body) }
  )

  def lambdaForm: Parser[ACST] = (
    (("(" ~> ("lambda" ~> ((("(" ~> rep(symbol)) <~ ")") ~ expression))) <~ ")" )
      ^^ { case symbols ~ body => LambdaForm(symbols, body) }
  )

  def forms:      Parser[ACST] =  ifForm | letForm | lambdaForm | form

  def expression: Parser[ACST] = literalInt | literalBoolean | symbol | forms

  def sourcecode: Parser[List[ACST]] = (expression *) <~ """$""".r
}

/** Simple Eval of AlphaLang */
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
  def update(bindings:List[BindingForm], env:List[(Symbol, ACST)]):List[(Symbol, ACST)] =  {
    if (bindings.isEmpty){
      env
    } else {
      bindings.head match {
        case BindingForm(symbol, body) => {
          update(bindings.tail, (symbol, evalSExp(body, env))::env)
        }
        case _ => throw new RuntimeException
      }
    }
  }

  /** evaluate s-expression */
  def evalSExp(expression: ACST, env:List[(Symbol, ACST)]):ACST = expression match {
    case IfForm(condtion, thenClause, elseClause) => { // if expression
      if (evalSExp(condtion, env).asInstanceOf[LiteralBoolean].value) (evalSExp(thenClause, env))
      else (evalSExp(elseClause, env))
    }
    case LetForm(binding, body) => {
      evalSExp(body, update(binding, env))
    }
    case LambdaForm(symbols, body) => {
      Closure(symbols, body, env)
    }
    case Form(operator, operands) => { // apply function
      val args = operands.map(arg => evalSExp(arg, env))
      val f = evalSExp(operator, env)
      f match {
        case primitiveInt: PrimitiveInt => primitiveInt.calcInts(args.asInstanceOf[List[LiteralInt]])
        case primitiveBoolean: PrimitiveBoolean => primitiveBoolean.calcBoolean(args.asInstanceOf[List[LiteralBoolean]])
        case Closure(symbols, body, env) => evalSExp(body, (symbols zip args) ++ env)
        case _ => throw new RuntimeException
      }
    }
    case symbol:Symbol => find(symbol, env)
    case primitive:Primitive => primitive
    case _ => {
      throw new RuntimeException
    }
  }
}

/** AlphaLang */
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
