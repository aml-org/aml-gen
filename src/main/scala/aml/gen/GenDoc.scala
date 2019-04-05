package aml.gen

import amf.core.vocabulary.Namespace.Xsd
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.{NodeMapping, PropertyMapping}
import org.scalacheck.Gen
import org.yaml.model._

object GenDoc {

  def doc(dialect: Dialect): Gen[YDocument] = {

    val root = dialect.documents().root().encoded().value()

    val nodes = dialect.declares.collect {
      case mapping: NodeMapping =>
        mapping.id -> node(mapping)
    }.toMap

    nodes(root).map { r =>
      YDocument(dialect.nameAndVersion())(r)
    }
  }

  private def node(node: NodeMapping): Gen[YMap] = {
    val props = Gen.sequence[IndexedSeq[YMapEntry], YMapEntry](node.propertiesMapping().map(prop))
    props.map(YMap(_, ""))
  }

  private def prop(property: PropertyMapping): Gen[YMapEntry] = {
    value(property).map(YMapEntry(property.name().value(), _))
  }

  private def value(property: PropertyMapping): Gen[YNode] = {
    property
      .literalRange()
      .option()
      .map(literal(_, property))
      .getOrElse(obj(property))
  }

  private def literal(range: String, property: PropertyMapping): Gen[YNode] = {
    range match {
      case v if (Xsd + "integer").iri() == v => integer(property)
      case v if (Xsd + "string").iri() == v  => string(property)
    }
  }

  private def obj(property: PropertyMapping): Gen[YNode] = {
    Gen.fail
  }

  private def string(property: PropertyMapping): Gen[YNode] = {
    Gen.alphaStr.map(YNode.fromString)
  }

  private def integer(property: PropertyMapping): Gen[YNode] = {
    Gen.posNum[Int].map(YNode.fromInt)
  }
}
