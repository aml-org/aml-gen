package aml.gen.context
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import aml.gen.GenDoc.GenLiteral

/** Context for generation. */
trait GenContext {

  /** Optionally return literal custom generator for a given PropertyMapping. */
  def lit(property: PropertyMapping): Option[GenLiteral]
}
