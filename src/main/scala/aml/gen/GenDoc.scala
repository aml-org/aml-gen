package aml.gen

import amf.plugins.document.vocabularies.model.document.Dialect
import org.scalacheck.Gen
import org.yaml.model.YDocument


object GenDoc {

//  def doc(dialect: Dialect): Gen[DialectInstance] // Ideally

    def doc(dialect: Dialect): Gen[YDocument] = {
      for {
        str <- Gen.alphaStr
      } yield {
        YDocument(dialect.nameAndVersion()) { b =>
          b.obj { b =>
            b.entry("a", str)
          }
        }
      }
    }
}
