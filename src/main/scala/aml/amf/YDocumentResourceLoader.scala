package aml.amf

import YDocumentResourceLoader.uri
import amf.client.remote.Content
import amf.core.lexer.CharSequenceStream
import amf.internal.resource.ResourceLoader
import org.yaml.model.YDocument
import org.yaml.render.YamlRender

import scala.concurrent.Future

class YDocumentResourceLoader(document: YDocument) extends ResourceLoader {

  override def fetch(resource: String): Future[Content] =
    Future.successful(Content(new CharSequenceStream(uri, YamlRender.render(document)), uri))

  override def accepts(resource: String): Boolean = resource == uri
}

object YDocumentResourceLoader {

  def apply(document: YDocument): ResourceLoader = new YDocumentResourceLoader(document)

  val uri = "memory://ydocument"
}
