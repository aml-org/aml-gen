package aml.commands

import aml.gen.commands.{CommandParser, GenerateCommand, ParserConfig}
import org.mulesoft.common.io.Fs
import org.scalatest.{AsyncFlatSpec, Matchers}

class CLIGenerationTest extends AsyncFlatSpec with Matchers {

  "Command generate" should "validate" in {
    val numberOfInstances = 6
    val args = Array(
      "generate",
      "src/test/resources/dialects/assets.yaml",
      "out/test/output",
      "-i",
      s"$numberOfInstances",
      "-c",
      "2",
      "-s",
      "7"
    )
    val optConfig: Option[ParserConfig] = CommandParser.parse(args)
    for {
      _ <- GenerateCommand(optConfig.get).execute()
    } yield {
      val outputDir          = Fs.syncFile("out/test/output")
      val generatedInstances = outputDir.list
      assert(outputDir.isDirectory)
      assert(generatedInstances.size == numberOfInstances)
      assert(generatedInstances.map(_.matches("instance-\\d{7}")).reduce(_ && _))
    }
  }
}
