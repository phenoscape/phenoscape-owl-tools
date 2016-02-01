package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.IRI
import org.phenoscape.kb.ingest.mgi.MGIAnatomyBridgeToEMAPA
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.apibinding.OWLManager
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLAxiom

object OBOUtil {

  val factory = OWLManager.getOWLDataFactory

  def iriForTermID(id: String): IRI = {
    if (id.startsWith("http://"))
      IRI.create(id)
    else
      IRI.create("http://purl.obolibrary.org/obo/" + id.replaceAll(":", "_"))
  }

  def zfinIRI(identifier: String): IRI = IRI.create("http://zfin.org/" + identifier)

  def mgiAnatomyIRI(identifier: String): IRI = IRI.create(MGIAnatomyBridgeToEMAPA.ontologyName + "#" + identifier.replaceAllLiterally(":", "_"))

  def mgiReferenceIRI(identifier: String): IRI = IRI.create("http://www.informatics.jax.org/reference/summary?id=" + identifier)

  def xenbaseImageIRI(identifier: String): IRI = IRI.create("http://www.xenbase.org/common/ViewImageActionNonAdmin.do?imageId=" + identifier.replaceFirst(".*IMG-", ""))

  def xenbaseArticleIRI(identifier: String): IRI = IRI.create(s"http://www.xenbase.org/literature/article.do?method=display&articleId=${identifier.replaceFirst(".*ART-", "")}")

  def createDefinedByAnnotation(term: OWLEntity): Option[OWLAnnotationAssertionAxiom] = {
    val iri = term.getIRI.toString
    if (iri.startsWith("http://purl.obolibrary.org/obo/")) {
      val prefix = iri.stripPrefix("http://purl.obolibrary.org/obo/").split("_", -1).dropRight(1).mkString("_")
      val ontIRI = "http://purl.obolibrary.org/obo/" + prefix.toLowerCase + ".owl"
      Option(term Annotation (factory.getRDFSIsDefinedBy, IRI.create(ontIRI)))
    } else None
  }

  def translatePostComposition(id: String): OWLClassExpression = PostCompositionParser.parseExpression(id).get

  def translatePostCompositionNamed(id: String): (OWLClass, Set[OWLAxiom]) = translatePostComposition(id) match {
    case named: OWLClass => (named, Set.empty)
    case expression      => ExpressionUtil.nameForExpressionWithAxioms(expression)
  }

}