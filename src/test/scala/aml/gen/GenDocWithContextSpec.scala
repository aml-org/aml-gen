package aml.gen

import amf.core.vocabulary.Namespace.XsdTypes
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import aml.amf.AmfOps
import aml.gen.context.GenContext
import org.scalacheck.Gen
import org.scalactic.anyvals.PosZInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{AsyncFlatSpec, Matchers}
import org.yaml.model.YNode
import org.yaml.render.YamlRender

class GenDocWithContextSpec extends AsyncFlatSpec with Matchers with AmfOps with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
    sizeRange = PosZInt(5))

  private val file = "literals.yaml"

  "GenDoc with context" should s"create a Gen[YDocument] for $file" in {
    for {
      dialect <- parse(s"/dialects/$file")
    } yield {
      val documents = GenDoc.doc(dialect, LiteralsGenContext)
      forAll(documents) { doc =>
        println(YamlRender.render(doc))
        doc.headComment should be(s"%${dialect.nameAndVersion()}")
        val obj         = doc.obj
        val str: String = obj.str
        str should be("Str example")
        val int: Int = obj.int
        int should be(100)
        val float: Double = obj.float
        float should be(200.0)
      }
    }
  }

  private def parse(dialect: String) = parseAml(s"file://$dialect", "Dialect 1.0").mapTo[Dialect]

  object LiteralsGenContext extends GenContext {

    override def lit(property: PropertyMapping): Option[Gen[YNode]] = property match {

      /** Example matching by name. */
      case propA if propA.name().option().contains("str") => Some(Gen.const("Str example"))

      /** Example matching by property mapping. */
      case propB if propB.nodePropertyMapping().option().contains("http://test.com/tmp#int") => Some(Gen.const(100))

      /** Example matching by property range. */
      case propC if propC.literalRange().option().contains(XsdTypes.xsdFloat.iri()) => Some(Gen.const(200.0))

      /** No match for other properties. */
      case _ => None
    }
  }
}
