from __future__ import division

def main():
	numoftaxa=int(sys.argv[1])


	# query for scores - necessary only after KB update.
	get_scores()
	
	# Load taxon, gene profile sizes and similarity scores
	taxonprofilesizes,geneprofilesizes,scores=load_profiles()
	sizes = [taxonprofilesizes,geneprofilesizes]
	
	
	# Run regression
	results=reg_m(scores,sizes)
	genecoeff=results.params[0]
	taxoncoeff=results.params[1]
	constant=results.params[2]
	
	# Plot residual plot - necessary only after KB update.
	plot_residuals(taxonprofilesizes, geneprofilesizes,scores,taxoncoeff,genecoeff,constant)
	

	# Compute studentized residuals
	studentizedresiduals= studentize(results)

	# Compute p-values and Expect scores
	compute_expect_scores(studentizedresiduals,numoftaxa)

def get_scores():
	resultdir="../results/"
	if not os.path.exists(resultdir):
		os.makedirs(resultdir)		
	query="curl -X POST --data-binary @getscores-URI.rq --header \"Content-Type:application/sparql-query\" --header \"Accept: text/tab-separated-values\" http://kb-dev.phenoscape.org/bigsparql > ../results/Scores_Gene_Taxon.tsv"
	os.system(query)
	size=loadprofilesizes()
	query_parse_results(size)

	

def loadprofilesizes():
	profilesize=dict()
	infile=open("../results/ProfileSizes.txt")
	for line in infile:
		entity,size=line.strip().split("\t")
		entity=entity.replace("#profile","")
		entity=entity.replace("http://purl.obolibrary.org/obo/","")
		profilesize[entity]=int(size)
	infile.close()
	return profilesize

def load_profiles():
	
	infile=open("../results/Scores_Sizes.txt")
	scores=[]
	geneprofilesizes=[]
	taxonprofilesizes=[]
	rawscores=[]
	for line in infile:
		if "Score" not in line:
			data=line.strip().split("\t")
			score=float(data[6])
			scores.append(score)
			geneprofilesizes.append(math.log(int(data[1])))
			taxonprofilesizes.append(math.log(int(data[4])))
			gene=data[2]
			taxon=data[5]
	infile.close()
	return taxonprofilesizes,geneprofilesizes,scores


	
def query_parse_results(size):
	scorefile=open("../results/Scores_Sizes.txt",'w')
	infile=open("../results/Scores_Gene_Taxon.tsv")
	topscores=dict()
	maxscore=dict()
	maxtaxon=dict()
	geneset=set()
	name=dict()
	taxonid=dict()
	scorefile.write("Gene\tGene Profile Size\tGene Name\tTaxon\tTaxon Profile Size\tTaxon Name\tScore\tURI\n")
	for line in infile:
		if "gene_label" not in line:
			uri, score, gene,genename,taxon,taxonname=line.strip().replace("\"","").replace("^^<http://www.w3.org/2001/XMLSchema#string>","").replace("^^<http://www.w3.org/2001/XMLSchema#double>","").replace("<","").replace(">","").replace("http://purl.obolibrary.org/obo/","").split("\t")
			scorefile.write(gene+"\t"+str(size[gene])+"\t"+genename+"\t"+taxon+"\t"+str(size[taxon])+"\t"+taxonname+"\t"+str(score)+"\t"+uri+ "\n")
			
	scorefile.close()

	
def studentize(results):
	print ("Doing studentization")
	influence = results.get_influence()
	studentizedresiduals=influence.get_resid_studentized_external()
	return studentizedresiduals

def compute_expect_scores(studentizedresiduals,numoftaxa):
	print "Computing p-values"
	ranks=open("../results/RankStatistics.txt",'w')
	ranks.write("URI\tStudentized Residuals\tp-value\tExpect Score\n")
	
	i=0
	infile=open("../results/Scores_Sizes.txt")
	for line in infile:
		if "Score" not in line:
			gene,genesize,genename,taxon,taxonsize,taxonname,score,uri=line.strip().split("\t")
			residual=studentizedresiduals[i]
			a=-residual*math.pi
			b=math.sqrt(6)-0.5772156649
			pvalue=1-math.exp(-math.exp(a/b))
			expect=pvalue*numoftaxa
			ranks.write(uri+"\t"+str(studentizedresiduals[i])+"\t"+str(round(pvalue,2))+"\t"+str(round(expect,2))+"\n")
			i+=1
	ranks.close()	
	infile.close()	


def reg_m(scores, sizes):
	print "Doing regression"
	ones = np.ones(len(sizes[0]))
	X = sm.add_constant(np.column_stack((sizes[0], ones)))
	for ele in sizes[1:]:
		X = sm.add_constant(np.column_stack((ele, X)))
	results = sm.OLS(scores, X).fit()
	return results



def plot_residuals(taxonprofilesizes, geneprofilesizes,scores,taxoncoeff,genecoeff,constant):
	geneaxis=[]
	taxonaxis=[]
	residuals=[]
	count=0
	mean=np.mean(scores)
	
	for i in range(0,len(scores)):
		genesize=geneprofilesizes[i]
		taxonsize=taxonprofilesizes[i]
		actualscore=scores[i]
		taxonaxis.append(taxonsize)
		geneaxis.append(genesize)
		predictedscore=constant+(taxoncoeff*taxonsize)+(genecoeff*genesize)	
		residual=actualscore-predictedscore
		residuals.append(residual)
		
	plt.scatter(np.array(geneaxis), np.array(residuals))
	plt.xlabel('Log(Gene Profile Size)')
	plt.ylabel('Residual')
	plt.savefig('../results/ResidualPlot_GeneSizes.png')

	plt.scatter(np.array(taxonaxis), np.array(residuals))
	plt.xlabel('Log(Taxon Profile Size)')
	plt.ylabel('Residual')
	plt.savefig('../results/ResidualPlot_TaxonSizes.png')
	


if __name__=='__main__':
	import sys
	import os
	from statsmodels.stats.outliers_influence import OLSInfluence
	import math
	import matplotlib.pyplot as plt
	import numpy as np
	import statsmodels.api as sm
	import statsmodels.stats.api as sms
	from scipy import stats
	main()