package aml.gen

import amf.AmfOps
import amf.plugins.document.vocabularies.model.document.Dialect
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{AsyncFlatSpec, Matchers}
import org.yaml.render.YamlRender

class GenDocSpec extends AsyncFlatSpec with Matchers with AmfOps with GeneratorDrivenPropertyChecks {

  private val literals: String = "/dialects/literals.yaml"
  private val basic: String    = "/dialects/basic.yaml"

  "GenDoc" should "create a Gen[YDocument] for a given Dialect" in {
    for {
      dialect <- parse(basic)
    } yield {
      val documents = GenDoc.doc(dialect)

      forAll(documents) { doc =>
        println(YamlRender.render(doc))
        doc.headComment should be("Literals 1.0")
      }
    }
  }

  private def parse(dialect: String) = parseAml(s"file://$dialect", "Dialect 1.0").mapTo[Dialect]
}
