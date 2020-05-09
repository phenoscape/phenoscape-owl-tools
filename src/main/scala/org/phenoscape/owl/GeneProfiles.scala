package org.phenoscape.owl

import org.apache.jena.query.Query
import org.openrdf.model.Statement
import org.openrdf.model.impl.{StatementImpl, URIImpl}
import org.openrdf.model.vocabulary.RDF
import org.openrdf.query.QueryLanguage
import org.openrdf.repository.sail.SailRepositoryConnection
import org.phenoscape.owl.Vocab._
import org.phenoscape.owl.util.SesameIterationIterator.iterationToIterator
import org.phenoscape.owlet.SPARQLComposer._

object GeneProfiles {

  def generateGeneProfiles(db: SailRepositoryConnection): Set[Statement] = {
    val query = db.prepareTupleQuery(QueryLanguage.SPARQL, genePhenotypesQuery.toString)
    (for {
      bindings <- query.evaluate
      phenotypeURIString = bindings.getValue("phenotype_class").stringValue
      geneURIString = bindings.getValue("gene").stringValue
      phenotypeURI = new URIImpl(phenotypeURIString)
      profileURI = new URIImpl(s"$geneURIString#profile")
      statement <- Set(
                     new StatementImpl(profileURI, RDF.TYPE, phenotypeURI),
                     new StatementImpl(
                       new URIImpl(geneURIString),
                       new URIImpl(has_phenotypic_profile.toString),
                       profileURI
                     )
                   )
    } yield statement).toSet
  }

  val genePhenotypesQuery: Query =
    select_distinct('gene, 'phenotype_class) from "http://kb.phenoscape.org/" where (bgp(
      t('annotation, rdfType, AnnotatedPhenotype),
      t('annotation, associated_with_gene, 'gene),
      t('annotation, rdfType, 'phenotype_class)
    ))

}
