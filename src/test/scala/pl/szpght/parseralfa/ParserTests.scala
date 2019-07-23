package pl.szpght.parseralfa

import org.scalatest._
import pl.szpght.parseralfa.Parser.FieldDeclaration

class ParserTests extends FlatSpec with Matchers {
  def tokenize(input: String) = Parser.tokenize(input)

  "parseFieldDeclaration" should "parse `field1: value1` as FieldDeclaration" in {
    val tokens = tokenize("field1: value1")
    val parsingResult = Parser.parseFieldDeclaration(tokens)
    parsingResult shouldBe Right(FieldDeclaration("field1", "value1"))
  }
}
