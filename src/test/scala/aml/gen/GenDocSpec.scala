package aml.gen

import amf.AmfOps
import amf.plugins.document.vocabularies.model.document.Dialect
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, AsyncFlatSpec, Matchers}
import org.yaml.model.YDocument
import org.yaml.render.YamlRender

import scala.concurrent.Await
import scala.concurrent.duration._

class GenDocSpec extends AsyncFlatSpec with Matchers with AmfOps with GeneratorDrivenPropertyChecks {

  private val literals: String = "/dialects/literals.yaml"
  private val basic: String    = "/dialects/basic.yaml"
  private val nested: String   = "/dialects/nested.yaml"
  private val res: String      = "/dialects/restrictions.yaml"

  private val fixture: Seq[String] = Seq(literals, basic, nested, res)

  fixture.foreach { file =>
    "GenDoc" should s"create a Gen[YDocument] for $file" in {
      for {
        dialect <- parse(file)
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
}
