package aml.gen.context
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import org.scalacheck.Gen
import org.yaml.model.YNode

/** GenContext with no custom defined generators. */
object EmptyGenContext extends GenContext {

  override def lit(property: PropertyMapping): Option[Gen[YNode]] = None
}
