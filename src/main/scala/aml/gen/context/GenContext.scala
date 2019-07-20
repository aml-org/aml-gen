package aml.gen.context
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import org.scalacheck.Gen
import org.yaml.model.YNode

/** Context for generation. */
trait GenContext {

  /** Optionally return literal custom Gen[YNode] for a given PropertyMapping. */
  def lit(property: PropertyMapping): Option[Gen[YNode]]
}
