package org.phenoscape.owl.util

import org.jdom2.{Element, Namespace}
import org.phenoscape.kb.ingest.util.OBOUtil
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLClassExpression, OWLObjectSomeValuesFrom}

import scala.collection.JavaConverters._

object PhenoXMLUtil {

  val phenoNS = Namespace.getNamespace("http://www.bioontologies.org/obd/schema/pheno")
  val factory = OWLManager.getOWLDataFactory

  case class EQ(entity: OWLClassExpression, quality: OWLClassExpression, relatedEntity: OWLClassExpression)

  def translatePhenotypeCharacter(phenotype: Element): EQ = {
    val bearer             = phenotype.getChild("bearer", phenoNS)
    val entityClass        = if (bearer != null) {
      val bearerType = bearer.getChild("typeref", phenoNS)
      if (bearerType != null) classFromTyperef(bearerType)
      else null
    } else null
    val quality            = phenotype.getChild("quality", phenoNS)
    val qualityClass       = if (quality != null) {
      val qualityType = quality.getChild("typeref", phenoNS)
      if (qualityType != null) classFromTyperef(qualityType)
      else null
    } else null
    val relatedEntity      =
      if (quality != null) quality.getChild("related_entity", phenoNS)
      else null
    val relatedEntityClass = if (relatedEntity != null) {
      val relatedEntityType = relatedEntity.getChild("typeref", phenoNS)
      if (relatedEntityType != null) classFromTyperef(relatedEntityType)
      else null
    } else null
    EQ(entityClass, qualityClass, relatedEntityClass)
  }

  def classFromTyperef(typeref: Element): OWLClassExpression = {
    val genusID    = typeref.getAttributeValue("about")
    val qualifiers = typeref.getChildren("qualifier", phenoNS)
    val genus      = factory.getOWLClass(OBOUtil.iriForTermID(genusID))
    if (qualifiers.isEmpty) genus
    else {
//      val operands: mutable.Set[OWLClassExpression] = mutable.Set(genus)
      val operands = qualifiers.asScala.map(restrictionFromQualifier).toSet[OWLClassExpression] + genus
      factory.getOWLObjectIntersectionOf(operands.asJava)
    }
  }

  def restrictionFromQualifier(qualifier: Element): OWLObjectSomeValuesFrom = {
    val propertyIRI = OBOUtil.iriForTermID(qualifier.getAttributeValue("relation"))
    val property    = factory.getOWLObjectProperty(propertyIRI)
    val filler      = classFromTyperef(qualifier.getChild("holds_in_relation_to", phenoNS).getChild("typeref", phenoNS))
    factory.getOWLObjectSomeValuesFrom(property, filler)
  }

}
