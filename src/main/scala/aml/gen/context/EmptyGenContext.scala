package aml.gen.context
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import aml.gen.GenDoc.GenLiteral

/** GenContext with no custom defined generators. */
object EmptyGenContext extends GenContext {

  override def lit(property: PropertyMapping): Option[GenLiteral] = None
}
