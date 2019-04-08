package aml.gen

import amf.core.vocabulary.Namespace.Xsd
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.{NodeMapping, PropertyMapping}
import aml.gen.GenDoc.{NodeGenerators, NodeMappings}
import org.scalacheck.Gen
import org.scalacheck.Gen.{const, frequency, some}
import org.yaml.model._

import scala.collection.mutable

class GenDoc private (val nodes: NodeGenerators, val mappings: NodeMappings) {

  def gen(dialect: Dialect): Gen[YDocument] = {

    mappings.values.foreach { m =>
      if (!nodes.contains(m.id)) {
        nodes.put(m.id, node(m))
      }
    }

    val root = dialect.documents().root().encoded().value()

    nodes(root).map { r =>
      YDocument(s"%${dialect.nameAndVersion()}")(r)
    }
  }

  private def node(node: NodeMapping): Gen[YMap] = {
    val props = Gen.sequence[IndexedSeq[Option[YMapEntry]], Option[YMapEntry]](node.propertiesMapping().map(prop))
    val gen   = props.map(entries => YMap(entries.flatten, ""))
    nodes.put(node.id, gen)
    gen
  }

  private def prop(property: PropertyMapping): Gen[Option[YMapEntry]] = {
    value(property).map(_.map(YMapEntry(property.name().value(), _)))
  }

  private def value(property: PropertyMapping): Gen[Option[YNode]] = {
    property
      .literalRange()
      .option()
      .map(literal(_, property))
      .getOrElse(obj(property))
  }

  private def literal(range: String, property: PropertyMapping): Gen[Option[YNode]] = {
    optional(property) {
      range match {
        case v if (Xsd + "integer").iri() == v => integer(property)
        case v if (Xsd + "string").iri() == v  => string(property)
        case v if (Xsd + "boolean").iri() == v => boolean(property)
      }
    }
  }

  private def obj(property: PropertyMapping): Gen[Option[YNode]] = {
    val range = property.objectRange().head.value() // Support for Unions?
    optional(property) {
      nodes
        .getOrElseUpdate(range, {
          node(mappings(range))
        })
        .map(YNode.fromMap)
    }
  }

  private def string(property: PropertyMapping): Gen[YNode] = {
    Gen.alphaStr.map(YNode.fromString)
  }

  private def integer(property: PropertyMapping): Gen[YNode] = {
    Gen.posNum[Int].map(YNode.fromInt)
  }

  private def boolean(property: PropertyMapping): Gen[YNode] = {
    Gen.oneOf(true, false).map(YNode.fromBool)
  }

  private def optional[T](property: PropertyMapping)(g: Gen[T]): Gen[Option[T]] = {
    property.minCount().value() match {
      case 0 => frequency(5 -> const(None), 5 -> some(g))
      case _ => some(g)
    }
  }
}

object GenDoc {

  type NodeGenerators = mutable.Map[String, Gen[YMap]]

  type NodeMappings = Map[String, NodeMapping]

  def doc(dialect: Dialect): Gen[YDocument] = {

    val mappings = dialect.declares.collect {
      case mapping: NodeMapping =>
        mapping.id -> mapping
    }.toMap

    new GenDoc(mutable.Map(), mappings).gen(dialect)
  }

}
