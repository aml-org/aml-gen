package aml.commands

import aml.gen.commands.{CommandParser, ParserConfig}
import org.scalatest.{FlatSpec, Matchers}

class CommandLineTest extends FlatSpec with Matchers {

  "Command generate" should "validate" in {
    val args = Array(
      "generate",
      "README.md",
      ".",
      "-i",
      "3",
      "-c",
      "2",
      "-s",
      "7"
    )
    val optConfig: Option[ParserConfig] = CommandParser.parse(args)
    assert(optConfig.isDefined)
  }

  "Command generate" should "fail with given args" in {
    val args = Array(
      "generate",
      "-i",
      "3",
      "-c",
      "2",
      "-s",
      "7"
    )
    val optConfig: Option[ParserConfig] = CommandParser.parse(args)
    assert(optConfig.isEmpty)
  }
}
