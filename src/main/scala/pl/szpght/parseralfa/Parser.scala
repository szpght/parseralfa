package pl.szpght.parseralfa

import scala.util.matching.Regex

object Parser {
  sealed trait ParseError
  case class UnexpectedToken(token: String) extends ParseError

  class Tokens(val tokens: List[Token]) {
    def lookAhead = tokens.head
    def accept = new Tokens(tokens.tail)
  }

  sealed trait Token
  case class Let() extends Token
  case class Eq() extends Token
  case class OpenBracket() extends Token
  case class CloseBracket() extends Token
  case class Comma() extends Token
  case class Colon() extends Token
  case class Assign() extends Token
  case class Symbol(symbol: String) extends Token
  case class InvalidToken(token: String) extends Token

  case class FieldDeclaration(fieldName: String, valueName: String)

  type Ast = Any // TODO
  type ParseResult = Either[ParseError, Ast]

  val LET = "let"
  val EQ = "="
  val OPENBRACKET = "{"
  val CLOSEBRACKET = "}"
  val COMMA = ","
  val COLON = ":"
  val ASSIGN = "<-"

  val delimiters = List(EQ, OPENBRACKET, CLOSEBRACKET, COLON, COMMA, ASSIGN)

  val tokenizePattern =
    delimiters
      .map(Regex.quote)
      .map(delim => s"((?<=$delim)|(?=$delim))")
      .reduce((a, b) => s"$a|$b")

  def isSymbol(token: String) = !delimiters.contains(token)

  def tokenize(program: String) = {
    val tokens = program
      .split(tokenizePattern)
      .map(_.trim)
      .filter(_.length > 0)
      .map(_ match {
        case LET => Let()
        case EQ => Eq()
        case OPENBRACKET => OpenBracket()
        case CLOSEBRACKET => CloseBracket()
        case COMMA => Comma()
        case COLON => Colon()
        case ASSIGN => Assign()
        case other => if (isSymbol(other)) Symbol(other) else InvalidToken(other)
      })
      .toList
    new Tokens(tokens)
  }

  def parseProgram(program: String) = {
    val tokens = tokenize(program)
  }

  def parseFieldDeclaration(tokens: Tokens) = {
    tokens.tokens match {
      case Seq(Symbol(variableName), Colon(), Symbol(valueName)) => Right(FieldDeclaration(variableName, valueName))
      case _ => Left(InvalidToken("here will be some info about first invalid token or sth"))
    }
  }

  def parserMain() = {
    val program = "pole1: wartosc1, pole2: wartosc2}"
    val hehe = tokenize(program)

    hehe.tokens.map(x => println(x))
  }
}