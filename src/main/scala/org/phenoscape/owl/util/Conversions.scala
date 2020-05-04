package org.phenoscape.owl.util

import better.files._
import scala.io.Source

object Conversions {

  implicit class BetterFileOps(val self: File) extends AnyVal {

    def toSource(encoding: String): Source =
      Source.fromFile(self.toJava, encoding)

  }

}
