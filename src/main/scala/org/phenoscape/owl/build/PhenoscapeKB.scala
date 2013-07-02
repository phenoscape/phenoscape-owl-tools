package org.phenoscape.owl.build

import java.io.BufferedReader
import java.io.StringReader
import scala.collection.JavaConversions._
import scala.collection.Set
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._
import org.nescent.strix.OWL._
import org.obolibrary.obo2owl.Obo2Owl
import org.obolibrary.oboformat.parser.OBOFormatParser
import org.phenoscape.owl.KnowledgeBaseBuilder
import org.phenoscape.owl.MaterializeInferences
import org.phenoscape.owl.NamedRestrictionGenerator
import org.phenoscape.owl.PropertyNormalizer
import org.phenoscape.owl.TaxonomyConverter
import org.phenoscape.owl.Vocab
import org.phenoscape.owl.mod.human.HumanPhenotypesToOWL
import org.phenoscape.owl.mod.mgi.MGIGeneticMarkersToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseExpressionToOWL
import org.phenoscape.owl.mod.xenbase.XenbaseGenesToOWL
import org.phenoscape.owl.mod.zfin.ZFINExpressionToOWL
import org.phenoscape.owl.mod.zfin.ZFINGeneticMarkersToOWL
import org.phenoscape.owl.mod.zfin.ZFINPhenotypesToOWL
import org.phenoscape.owl.mod.zfin.ZFINPreviousGeneNamesToOWL
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.OWLReasoner
import eu.trowl.owlapi3.rel.reasoner.dl.RELReasonerFactory
import java.io.File
import org.apache.commons.io.FileUtils
import org.phenoscape.owl.PhenexToOWL
import org.apache.commons.lang3.StringUtils
import org.semanticweb.owlapi.model.OWLClassAxiom

object PhenoscapeKB extends KnowledgeBaseBuilder {

    val cwd = System.getProperty("user.dir");
    val STAGING = new File("staging");
    val KB = new File("staging/kb");
    val NEXML = new File("staging/nexml");
    STAGING.mkdir();
    KB.mkdir();
    NEXML.mkdir();
    cd(KB);

    val uberon = load("http://purl.obolibrary.org/obo/uberon/merged.owl");
    val uberonReferences = load("http://purl.obolibrary.org/obo/uberon/references/references.owl");
    val ext = load("http://purl.obolibrary.org/obo/uberon/ext.owl");
    write(combine(uberon, ext, uberonReferences), "uberon.owl");
    val pato = load("http://purl.obolibrary.org/obo/pato.owl");
    write(pato, "pato.owl");
    val bspo = load("http://purl.obolibrary.org/obo/bspo.owl");
    write(bspo, "bspo.owl");
    val go = load("http://purl.obolibrary.org/obo/go.owl");
    write(go, "go.owl");
    val vto = load("http://purl.obolibrary.org/obo/vto.owl");
    write(vto, "vto.owl");
    val zfa = load("http://purl.obolibrary.org/obo/zfa.owl");
    write(zfa, "zfa.owl");
    val xao = load("http://purl.obolibrary.org/obo/xao.owl");
    write(xao, "xao.owl");
    val hp = load("http://purl.obolibrary.org/obo/hp.owl");
    write(hp, "hp.owl");

    val hpEQOBO = Source.fromURL("http://purl.obolibrary.org/obo/hp/hp-equivalence-axioms.obo", "utf-8").mkString;
    val hpEQOBOInvolves = hpEQOBO.replaceFirst("ontology: hp/hp-logical-definitions", "ontology: hp/hp-logical-definitions\nlogical-definition-view-relation: involves");
    val hpEQ = new Obo2Owl().convert(new OBOFormatParser().parse(new BufferedReader(new StringReader(hpEQOBOInvolves))));
    write(hpEQ, "hp-logical-definitions.owl");

    val zfaToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-ext-bridge-to-zfa.owl");
    write(zfaToUberon, "uberon-ext-bridge-to-zfa.owl");
    val xaoToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-xao.owl");
    write(xaoToUberon, "uberon-bridge-to-xao.owl");
    val fmaToUberon = load("http://purl.obolibrary.org/obo/uberon/bridge/uberon-bridge-to-fma.owl");
    write(fmaToUberon, "uberon-bridge-to-fma.owl");

    val coreReasoner = reasoner(List(uberon, ext, pato, bspo, go));
    val anatomicalEntities = coreReasoner.getSubClasses(Class(Vocab.ANATOMICAL_ENTITY), false).getFlattened();
    val qualities = coreReasoner.getSubClasses(Class(Vocab.QUALITY), false).getFlattened();

    val vtoIndividuals = TaxonomyConverter.createInstanceOntology(vto);
    write(vtoIndividuals, "vto-individuals.owl");
    val vtoReasoner = new RELReasonerFactory().createReasoner(vtoIndividuals);
    val materializedVTO = manager.createOntology();
    MaterializeInferences.materializeInferences(materializedVTO, vtoReasoner);
    write(materializedVTO, "vto-individuals-closure.owl");

    cd(STAGING);
    Seq("svn", "checkout", "https://github.com/phenoscape/phenoscape-data/trunk/Curation Files", "nexml") !!;
    cd(NEXML);
    val filesToConvert = (FileUtils.listFiles(new File("completed-phenex-files"), Array("xml"), true) ++ 
            FileUtils.listFiles(new File("fin_limb-incomplete-files"), Array("xml"), true)).filterNot(_.getName() == "catalog-v001.xml");
    cd(KB);
    val nexmlTBoxAxioms: Set[OWLAxiom] = mutable.Set();
    filesToConvert.foreach(file => {
        val nexOntology = PropertyNormalizer.normalize(PhenexToOWL.convert(file));
        MaterializeInferences.materializeInferences(nexOntology); //TODO may be able to remove this with tweaks to Phenex converter
        nexmlTBoxAxioms.addAll(nexOntology.getTBoxAxioms(false));
        write(nexOntology, file.getName().replaceAll(".xml$", ".owl"));
    });


    val zfinGenes = PropertyNormalizer.normalize(ZFINGeneticMarkersToOWL.convert(Source.fromURL("http://zfin.org/downloads/genetic_markers.txt", "ISO-8859-1")));
    write(zfinGenes, "zfin-genes.owl");
    val zfinPreviousGeneNames = PropertyNormalizer.normalize(ZFINPreviousGeneNamesToOWL.convert(Source.fromURL("http://zfin.org/downloads/aliases.txt", "ISO-8859-1")));
    write(zfinPreviousGeneNames, "zfin-previous-gene-names.owl");    
    val zfinExpressionData = PropertyNormalizer.normalize(ZFINExpressionToOWL.convert(Source.fromURL("http://zfin.org/downloads/wildtype-expression.txt", "ISO-8859-1")));
    write(zfinExpressionData, "zfin-expression-data.owl");    
    val zfinPhenotypeData = PropertyNormalizer.normalize(ZFINPhenotypesToOWL.convert(Source.fromURL("https://zfin.org/downloads/phenoGeneClean.txt", "ISO-8859-1")));
    write(zfinPhenotypeData, "zfin-phenotype-data.owl");


    val mgiGenes = PropertyNormalizer.normalize(MGIGeneticMarkersToOWL.convert(Source.fromURL("ftp://ftp.informatics.jax.org/pub/reports/MRK_List2.rpt", "utf-8")));
    write(mgiGenes, "mgi-genes.owl");
    //val mgiExpressionData = PropertyNormalizer.normalize(MGIExpressionToOWL.convert(Source.fromURL()));
    //write(mgiExpressionData, "mgi-expression-data.owl");
    //val mgiPhenotypeData = PropertyNormalizer.normalize(MGIPhenotypesToOWL.convert(Source.fromURL()));
    //write(mgiPhenotypeData, "mgi-phenotype-data.owl");


    val xenbaseGenes = PropertyNormalizer.normalize(XenbaseGenesToOWL.convert(Source.fromURL("ftp://ftp.xenbase.org/pub/GenePageReports/GenePageGeneralInfo_ManuallyCurated.txt")));
    write(xenbaseGenes, "xenbase-genes.owl");
    val xenbaseExpressionData = PropertyNormalizer.normalize(XenbaseExpressionToOWL.convert(
            Source.fromURL("ftp://ftp.xenbase.org/pub/GenePageReports/XenbaseGenepageToGeneIdMapping.txt"),
            Source.fromURL("ftp://ftp.xenbase.org/pub/GenePageReports/GeneExpression_laevis.txt"), 
            Source.fromURL("ftp://ftp.xenbase.org/pub//GenePageReports/GeneExpression_tropicalis.txt")));
    write(xenbaseExpressionData, "xenbase-expression-data.owl");


    val humanPhenotypeData = PropertyNormalizer.normalize(HumanPhenotypesToOWL.convert(Source.fromURL("http://compbio.charite.de/hudson/job/hpo.annotations.monthly/lastStableBuild/artifact/annotation/genes_to_phenotype.txt", "utf-8")));
    write(humanPhenotypeData, "human-phenotypes.owl");

    val tboxFromData = manager.createOntology(
            zfinGenes.getTBoxAxioms(false) ++ 
            zfinPreviousGeneNames.getTBoxAxioms(false) ++
            zfinExpressionData.getTBoxAxioms(false) ++
            zfinPhenotypeData.getTBoxAxioms(false) ++
            mgiGenes.getTBoxAxioms(false) ++
            xenbaseGenes.getTBoxAxioms(false) ++
            xenbaseExpressionData.getTBoxAxioms(false) ++
            humanPhenotypeData.getTBoxAxioms(false) ++ 
            nexmlTBoxAxioms);

    val parts = manager.createOntology(anatomicalEntities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.PART_OF), _)).toSet[OWLAxiom]);
    val bearers = manager.createOntology(qualities.map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.BEARER_OF), _)).toSet[OWLAxiom]);
    val involvers = manager.createOntology((anatomicalEntities ++ qualities).map(NamedRestrictionGenerator.createRestriction(ObjectProperty(Vocab.INVOLVES), _)).toSet[OWLAxiom]);

    val allTBox = List(uberon, ext, pato, bspo, go, vto, zfa, xao, hp, 
            hpEQ, zfaToUberon, xaoToUberon, fmaToUberon, parts, bearers, involvers, tboxFromData);
    val tboxReasoner = reasoner(allTBox);
    val inferredAxioms = manager.createOntology();
    MaterializeInferences.materializeInferences(inferredAxioms, tboxReasoner);
    write(combine(parts, bearers, involvers, inferredAxioms), "generated.owl");

}