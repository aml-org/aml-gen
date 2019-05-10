package aml.gen.commands

import org.mulesoft.common.io.{Fs, SyncFile}
import scopt.OptionParser
import scala.util.Random

case class ParserConfig(dialect: Option[SyncFile] = None,
                        output: Option[SyncFile] = None,
                        instances: Int = 1000,
                        cardinality: Int = 3,
                        seed: Long = Random.nextLong())

object CommandParser {

  val parser: OptionParser[ParserConfig] = new scopt.OptionParser[ParserConfig]("amlgen") {
    head("AML", "1.0")
    cmd("generate")
      .text("Generates dialect instances")
      .children {
        arg[String]("dialect")
          .action((path, c) => c.copy(dialect = Option(Fs.syncFile(path))))
          .text("dialect is the path to the dialect")
        arg[String]("output")
          .action((path, c) => c.copy(output = Option(Fs.syncFile(path))))
          .text("output is the path to the folder where the generated files will be dumped")
        opt[Int]('i', "instances")
          .action((i, c) => c.copy(instances = i))
          .text("instances is the number of dialect instances you want to generate. Default is 1000")
        opt[Int]('c', "cardinality")
          .action((i, c) => c.copy(cardinality = i))
          .text("cardinality is the maximum length of generated lists and maps. Default is 3")
        opt[Long]('s', "seed")
          .action((l, c) => c.copy(seed = l))
          .text("the seed used when generating files. Default is random")
      }

    checkConfig(c => {
      val builder: StringBuilder = StringBuilder.newBuilder
      c.dialect match {
        case Some(d) =>
          if (!d.exists) builder.append(s"The path to the dialect provided doesn't exists: ${d.path}\n")
          if (d.isDirectory) builder.append(s"The path to the dialect is a folder: ${d.path}\n")
        case _ => builder.append("Missing <dialect> path\n")
      }
      if (c.output.isEmpty) builder.append("Missing <output> path\n")
      if (builder.isEmpty) success else failure(builder.toString())
    })
  }

  def parse(args: Array[String]): Option[ParserConfig] = parser.parse(args, ParserConfig())
}
