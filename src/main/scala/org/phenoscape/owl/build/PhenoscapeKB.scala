package org.phenoscape.owl.build

import scala.collection.JavaConversions._
import scala.collection.Set
import scala.io.Source

import org.nescent.strix.OWL._
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.mod.zfin.ZFINGeneticMarkersToOWL
import org.phenoscape.owl.mod.zfin.ZFINPreviousGeneNamesToOWL
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.OWLReasoner

object PhenoscapeKB extends KnowledgeBaseBuilder {

    //FIXME all managers need to be no-import or handle imports specially
    val manager = OWLManager.createOWLOntologyManager();

    val uberon = load("http://purl.obolibrary.org/obo/uberon/merged.owl");
    val ext = load("http://purl.obolibrary.org/obo/uberon/ext.owl");
    write(combine(uberon, ext), "uberon.owl");
    val pato = load("http://purl.obolibrary.org/obo/pato.owl");
    write(pato, "pato.owl");
    val bspo = load("http://purl.obolibrary.org/obo/bspo.owl");
    write(bspo, "bspo.owl");
    val go = load("http://purl.obolibrary.org/obo/go.owl");
    write(go, "go.owl");
    val zfa = load("http://purl.obolibrary.org/obo/zfa.owl");
    write(zfa, "zfa.owl");
    val xao = load("http://purl.obolibrary.org/obo/xao.owl");
    write(xao, "xao.owl");
    val hp = load("http://purl.obolibrary.org/obo/hp.owl");
    write(hp, "hp.owl");
    //val hpEQ = ... //TODO

    
    val zfaToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl");
    write(zfaToUberon, "uberon-ext-bridge-to-zfa.owl");
    val xaoToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl");
    write(xaoToUberon, "uberon-bridge-to-xao.owl");
    val fmaToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-fma.owl");
    write(fmaToUberon, "uberon-bridge-to-fma.owl");

    val coreReasoner = reasoner(List(uberon, ext, pato, bspo, go));
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened();
    val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened();

    val parts = manager.createOntology(anatomicalEntities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.PART_OF), _)).toSet[OWLAxiom]);
    val bearers = manager.createOntology(qualities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.BEARER_OF), _)).toSet[OWLAxiom]);
    val involvers = manager.createOntology((anatomicalEntities ++ qualities).map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.INVOLVES), _)).toSet[OWLAxiom]);

    val zfinGeneticMarkers = PropertyNormalizer.normalize(ZFINGeneticMarkersToOWL.convert(Source.fromURL("http://zfin.org/downloads/genetic_markers.txt", "ISO-8859-1")));
    write(zfinGeneticMarkers, "zfin-genetic-markers.owl");
    val zfinPreviousGeneNames = PropertyNormalizer.normalize(ZFINPreviousGeneNamesToOWL.convert(Source.fromURL("http://zfin.org/downloads/aliases.txt", "ISO-8859-1")));
    write(zfinPreviousGeneNames, "zfin-previous-gene-names.owl");

    //TODO get class axioms from data files to add to tbox
    val allTBox = List(uberon, ext, pato, bspo, go, zfa, xao, hp, 
        zfaToUberon, xaoToUberon, fmaToUberon, parts, bearers, involvers);
    val tboxReasoner = reasoner(allTBox);
    val inferredAxioms = manager.createOntology();
    MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner);
    write(combine(parts, bearers, involvers, inferredAxioms), "generated.owl");

}