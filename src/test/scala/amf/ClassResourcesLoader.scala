package amf

import java.io.Closeable

import amf.client.remote.Content
import amf.core.lexer.CharSequenceStream
import amf.internal.resource.ResourceLoader

import scala.concurrent.Future
import scala.io.Source.fromInputStream

case class ClassResourcesLoader() extends ResourceLoader {
  override def fetch(resource: String): Future[Content] = {
    Future.successful {
      val stream = getClass.getResourceAsStream(sanitize(resource))
      val source = using(fromInputStream(stream))(_.mkString)
      Content(
        new CharSequenceStream(source),
        resource
      )
    }
  }

  private def using[A <: Closeable, B](resource: A)(f: A => B): B = {
    try f(resource)
    finally resource.close()
  }

  override def accepts(resource: String): Boolean = {
    val sanitized = sanitize(resource)
    getClass.getResource(sanitized) != null
  }

  private def sanitize(resource: String) = resource match {
    case _ => s"${resource.stripPrefix("file://")}"
  }
}
