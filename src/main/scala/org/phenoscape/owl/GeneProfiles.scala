package org.phenoscape.owl

import org.openrdf.repository.sail.SailRepositoryConnection
import org.phenoscape.owl.Vocab._
import org.phenoscape.owlet.SPARQLComposer._
import com.hp.hpl.jena.query.Query
import org.openrdf.query.QueryLanguage
import org.phenoscape.owl.util.SesameIterationIterator.iterationToIterator
import org.openrdf.model.impl.URIImpl
import org.openrdf.model.impl.StatementImpl
import org.openrdf.model.Statement
import org.openrdf.model.vocabulary.RDF

object GeneProfiles {

  def generateGeneProfiles(db: SailRepositoryConnection): Set[Statement] = {
    val query = db.prepareTupleQuery(QueryLanguage.SPARQL, genePhenotypesQuery.toString)
    query.evaluate().map { bindings =>
      val geneURIString = bindings.getValue("gene").stringValue
      val phenotypeURI = new URIImpl(bindings.getValue("phenotype_class").stringValue)
      val profileURI = new URIImpl(geneURIString + "#profile")
      Set(new StatementImpl(profileURI, RDF.TYPE, phenotypeURI),
        new StatementImpl(new URIImpl(geneURIString), new URIImpl(has_phenotypic_profile.toString), profileURI))
    }.flatten.toSet
  }

  val genePhenotypesQuery: Query =
    select_distinct('gene, 'phenotype_class) from "http://kb.phenoscape.org/" where (
      bgp(
        t('annotation, rdfType, AnnotatedPhenotype),
        t('annotation, associated_with_gene, 'gene),
        t('annotation, rdfType, 'phenotype_class)))

}