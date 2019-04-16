package aml.gen

import java.text.SimpleDateFormat
import java.util.Date

import amf.core.model.domain.DomainElement
import amf.core.vocabulary.Namespace.{Shapes, Xsd}
import amf.plugins.document.vocabularies.model.document.{Dialect, DialectLibrary}
import amf.plugins.document.vocabularies.model.domain.{NodeMappable, NodeMapping, PropertyMapping, UnionNodeMapping}
import aml.gen.GenDoc.{NodeGenerators, NodeMappables}
import org.scalacheck.Gen.{const, frequency, some}
import org.scalacheck.{Arbitrary, Gen}
import org.yaml.model._
import wolfendale.scalacheck.regexp.RegexpGen

import scala.collection.mutable

case class GenDoc private (nodes: NodeGenerators, mappings: NodeMappables) {

  private val genDate        = Gen.chooseNum(-2208988800000L, 32503680000000L) map { new Date(_) }
  private val datetimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  private val dateFormat     = new SimpleDateFormat("yyyy-MM-dd")

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

  private def node(node: NodeMappable): Gen[YMap] = node match {
    case n: NodeMapping      => nodeMapping(n)
    case u: UnionNodeMapping => unionNode(u)
  }

  private def unionNode(union: UnionNodeMapping): Gen[YMap] = {
    val gen = Gen.oneOf(union.objectRange().map(o => mappings(o.value()))).flatMap(node)
    nodes.put(union.id, gen)
    gen
  }

  private def nodeMapping(node: NodeMapping): Gen[YMap] = {
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
      multiple(property) {
        range match {
          case v if (Xsd + "boolean").iri() == v  => boolean(property)
          case v if (Xsd + "integer").iri() == v  => integer(property)
          case v if (Xsd + "string").iri() == v   => string(property)
          case v if (Xsd + "float").iri() == v    => double(property)
          case v if (Xsd + "double").iri() == v   => double(property)
          case v if (Shapes + "link").iri() == v  => link(property)
          case v if (Xsd + "anyUri").iri() == v   => link(property)
          case v if (Xsd + "date").iri() == v     => date(property)
          case v if (Xsd + "dateTime").iri() == v => datetime(property)
          case _                                  => string(property)
        }
      }
    }
  }

  private def obj(property: PropertyMapping): Gen[Option[YNode]] = {
    val range = property.objectRange().head.value()
    optional(property) {
      multiple(property) {
        nodes.getOrElseUpdate(range, node(mappings(range))).map(YNode.fromMap)
      }
    }
  }

  private def date(property: PropertyMapping): Gen[YNode] =
    genDate.map(gDate => {
      val s = YScalar(dateFormat.format(gDate))
      YNode(s, YType.Timestamp.tag, sourceName = s.sourceName)
    })

  private def datetime(property: PropertyMapping): Gen[YNode] = {
    genDate.map(date => {
      val s = YScalar(datetimeFormat.format(date))
      YNode(s, YType.Timestamp.tag, sourceName = s.sourceName)
    })
  }

  private def link(property: PropertyMapping): Gen[YNode] = {
    RegexpGen.from("https?://[a-zA-Z]+\\.com").map(YNode.fromString)
  }

  private def double(property: PropertyMapping): Gen[YNode] = {
    val minValue = property.minimum().option()
    val maxValue = property.maximum().option()
    Gen
      .chooseNum[Double](minValue.getOrElse(Double.MinValue), maxValue.getOrElse(Double.MaxValue))
      .map(YNode.fromDouble)
  }

  private def string(property: PropertyMapping): Gen[YNode] = {
    val patternValue = property.pattern().option()
    val stringGen    = patternValue.map(RegexpGen.from).getOrElse(Gen.alphaStr)
    stringGen.map(YNode.fromString)
  }

  private def integer(property: PropertyMapping): Gen[YNode] = {
    val minValue = property.minimum().option().map(_.toInt)
    val maxValue = property.maximum().option().map(_.toInt)
    Gen.chooseNum[Int](minValue.getOrElse(Int.MinValue), maxValue.getOrElse(Int.MaxValue)).map(YNode.fromInt)
  }

  private def boolean(property: PropertyMapping): Gen[YNode] = {
    Arbitrary.arbBool.arbitrary.map(YNode.fromBool)
  }

  private def multiple(property: PropertyMapping)(g: Gen[YNode]): Gen[YNode] = {
    if (property.allowMultiple().value()) {
      val gen = if (property.minCount().value() != 0) Gen.nonEmptyListOf(g) else Gen.listOf(g)
      gen.map(YSequence.apply(_: _*))
    } else {
      g
    }
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

  type NodeMappables = mutable.Map[String, NodeMappable]

  def doc(dialect: Dialect): Gen[YDocument] = {

    val mappings: NodeMappables = mutable.Map()

    def collect(declared: Seq[DomainElement]): Unit = {
      declared.foreach {
        case m: NodeMappable => mappings.put(m.id, m)
      }
    }

    dialect.references.foreach {
      case ref: DialectLibrary => collect(ref.declares)
    }

    collect(dialect.declares)

    GenDoc(mutable.Map(), mappings).gen(dialect)
  }
}
