package aml.gen.commands

import java.text.DecimalFormat

import amf.plugins.document.vocabularies.model.document.Dialect
import aml.amf.AmfOps
import aml.gen.GenDoc
import org.mulesoft.common.io.Fs
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import org.yaml.model.YDocument
import org.yaml.render.YamlRender

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FutureCommand {
  def execute(): Future[Unit]
}

case class GenerateCommand private (cfg: ParserConfig) extends FutureCommand with AmfOps {

  private var offset   = 0
  private final val df = new DecimalFormat("0000000")

  override def execute(): Future[Unit] = {
    for {
      dialect <- parseAml(s"file://${cfg.dialect.get.path}", "Dialect 1.0").mapTo[Dialect]
    } yield {
      val documents = GenDoc.doc(dialect)
      val outputDir = cfg.output.get
      if (!outputDir.exists) outputDir.mkdir
      for (i <- 1 to cfg.instances) {
        val f = Fs.syncFile(outputDir, s"instance-${df.format(i)}")
        f.write(YamlRender.render(generateDocument(documents, i)))
      }
    }
  }

  @tailrec private def generateDocument(gen: Gen[YDocument], i: Int): YDocument = {
    gen(Parameters.default.withSize(cfg.cardinality), Seed(cfg.seed + i + offset)) match {
      case Some(doc) => doc
      case _ =>
        offset += 1
        generateDocument(gen, i)
    }
  }
}
