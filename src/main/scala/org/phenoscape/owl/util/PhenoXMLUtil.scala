package org.phenoscape.owl.util

import org.semanticweb.owlapi.model.OWLClassExpression
import org.jdom2.Element
import org.jdom2.Namespace
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
import scala.collection.mutable
import scala.collection.JavaConverters._
import org.phenoscape.kb.ingest.util.OBOUtil

object PhenoXMLUtil {

  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno")
  val factory = OWLManager.getOWLDataFactory

  case class EQ(
    val entity: OWLClassExpression,
    val quality: OWLClassExpression,
    val relatedEntity: OWLClassExpression
  ) {}

  def translatePhenotypeCharacter(phenotype: Element): EQ = {
    val bearer = phenotype.getChild("bearer", phenoNS)
    val entityClass = if (bearer != null) {
      val bearerType = bearer.getChild("typeref", phenoNS)
      if (bearerType != null)
        classFromTyperef(bearerType)
      else
        null
    } else
      null
    val quality = phenotype.getChild("quality", phenoNS)
    val qualityClass = if (quality != null) {
      val qualityType = quality.getChild("typeref", phenoNS)
      if (qualityType != null)
        classFromTyperef(qualityType)
      else
        null

    } else
      null
    val relatedEntity =
      if (quality != null)
        quality.getChild("related_entity", phenoNS)
      else
        null
    val relatedEntityClass = if (relatedEntity != null) {
      val relatedEntityType = relatedEntity.getChild("typeref", phenoNS)
      if (relatedEntityType != null)
        classFromTyperef(relatedEntityType)
      else
        null
    } else
      null
    return EQ(entityClass, qualityClass, relatedEntityClass)
  }

  def classFromTyperef(typeref: Element): OWLClassExpression = {
    val genusID = typeref.getAttributeValue("about")
    val qualifiers = typeref.getChildren("qualifier", phenoNS)
    val genus = factory.getOWLClass(OBOUtil.iriForTermID(genusID))
    if (qualifiers.isEmpty())
      return genus
    else {
//      val operands: mutable.Set[OWLClassExpression] = mutable.Set(genus)
      val operands = qualifiers.asScala.map(restrictionFromQualifier(_)).toSet[OWLClassExpression] + genus
      return factory.getOWLObjectIntersectionOf(operands.asJava)
    }
  }

  def restrictionFromQualifier(qualifier: Element): OWLObjectSomeValuesFrom = {
    val propertyIRI = OBOUtil.iriForTermID(qualifier.getAttributeValue("relation"))
    val property = factory.getOWLObjectProperty(propertyIRI)
    val filler = classFromTyperef(qualifier.getChild("holds_in_relation_to", phenoNS).getChild("typeref", phenoNS))
    return factory.getOWLObjectSomeValuesFrom(property, filler)
  }

}
