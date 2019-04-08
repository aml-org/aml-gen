package amf
import amf.core.AMFSerializer
import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.core.remote._
import amf.core.services.{RuntimeCompiler, RuntimeValidator}
import amf.core.unsafe.PlatformSecrets
import amf.internal.environment.Environment
import amf.plugins.document.Vocabularies
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.features.AMFValidation
import org.yaml.model.YDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait AmfOps extends PlatformSecrets {

  Vocabularies.register()
  AMFValidation.register()
  Core.registerPlugin(AMLPlugin)
  Core.init().get

  val environment: Environment = Environment().add(ClassResourcesLoader())

  def parseAml(uri: String, dialect: String): Future[BaseUnit] = {
    for {
      unit   <- parse(uri, "application/yaml", Aml)
      result <- RuntimeValidator(unit, ProfileName(dialect))
    } yield {
      if (!result.conforms) throw new IllegalStateException(result.toString())
      unit
    }
  }

  def parseAml(document: YDocument, dialect: String): Future[BaseUnit] = {
    for {
      unit   <- parse(document, "application/yaml", Aml)
      result <- RuntimeValidator(unit, ProfileName(dialect))
    } yield {
      if (!result.conforms) throw new IllegalStateException(result.toString())
      unit
    }
  }

  def parseRaml(api: String): Future[BaseUnit] = parse(api, "application/raml", Raml)

  def render(unit: BaseUnit): Future[String] = {
    new AMFSerializer(unit, "application/ld+json", Aml.name, RenderOptions().withoutSourceMaps.withCompactUris).renderToString
  }

  def parse(document: YDocument, mediaType: String, vendor: Vendor): Future[BaseUnit] =
    RuntimeCompiler(
      YDocumentResourceLoader.uri,
      Some(mediaType),
      Some(vendor.name),
      Context(platform),
      env = Environment(YDocumentResourceLoader(document)),
      cache = Cache()
    )

  def parse(uri: String, mediaType: String, vendor: Vendor): Future[BaseUnit] =
    RuntimeCompiler(uri, Some(mediaType), Some(vendor.name), Context(platform), env = environment, cache = Cache())
}
