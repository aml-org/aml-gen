package aml.gen

import amf.plugins.document.vocabularies.model.document.Dialect
import aml.amf.AmfOps
import org.mulesoft.common.io.Fs
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, AsyncFlatSpec, Matchers}
import org.yaml.model.YDocument
import org.yaml.render.YamlRender

import scala.concurrent.Await
import scala.concurrent.duration._

class GenDocSpec extends AsyncFlatSpec with Matchers with AmfOps with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 0, maxSize = 5)
  private val file                            = System.getProperty("yaml")
  private val dialects =
    if (file != null) Array(file)
    else
      files("src/test/resources/dialects").filter(!_.endsWith("ignored"))

  dialects.foreach { file =>
    "GenDoc" should s"create a Gen[YDocument] for $file" in {
      for {
        dialect <- parse(s"/dialects/$file")
      } yield {
        val documents = GenDoc.doc(dialect)
        forAll(documents) { doc =>
          println(YamlRender.render(doc))
          doc.headComment should be(s"%${dialect.nameAndVersion()}")
          validate(dialect, doc)
        }
      }
    }
  }

  private def validate(dialect: Dialect, document: YDocument): Assertion = {
    // Until https://github.com/scalatest/scalatest/issues/1370
    Await.result(parseAml(document, dialect.nameAndVersion()), 600 second)
    succeed
  }

  private def parse(dialect: String) = parseAml(s"file://$dialect", "Dialect 1.0").mapTo[Dialect]

  private def files(directory: String) = Fs.syncFile(directory).list.filter(_.contains(".yaml"))
}
