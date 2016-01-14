from __future__ import division

def main():
	size_of_corpus=int(sys.argv[1])

	get_scores()

	# Load corpus, query profile sizes and similarity scores
	corpus_profile_sizes, query_profile_sizes, scores = load_profiles()
	sizes = [corpus_profile_sizes, query_profile_sizes]
	
	
	# Run regression
	results = reg_m(scores, sizes)
	query_coeff = results.params[0]
	corpus_coeff = results.params[1]
	constant = results.params[2]
	
	# Plot residual plot - uncomment if profile sizes or similarity scores have changed due to updated data.
	
	#plot_residuals(corpus_profile_sizes, query_profile_sizes,scores,corpus_coeff,query_coeff,constant)
	

	# Compute studentized residuals
	studentizedresiduals = studentize(results)

	# Compute p-values and Expect scores
	compute_expect_scores(studentizedresiduals, size_of_corpus)


def get_scores():
	# Uncomment if similarity scores have changed due to updated data

	#query="curl -X POST --data-binary @getscores-URI.rq --header \"Content-Type:application/sparql-query\" --header \"Accept: text/tab-separated-values\" http://kb-dev.phenoscape.org/bigsparql > ../results/Scores.tsv"
	#os.system(query)
	size = loadprofilesizes()
	query_parse_results(size)


def loadprofilesizes():
	profilesize = dict()
	infile = open("ProfileSizes.txt")
	for line in infile:
		entity, size = line.strip().split("\t")
		entity = entity.replace("#profile", "")
		profilesize[entity] = int(size)
	infile.close()
	return profilesize


def load_profiles():
	infile = open("Scores_Sizes.txt")
	scores = []
	query_profile_sizes = []
	corpus_profile_sizes = []
	rawscores = []
	for line in infile:
		if "Score" not in line:
			data = line.strip().split("\t")
			score = float(data[6])
			scores.append(score)
			queryprofilesizes.append(math.log(int(data[1])))
			corpusprofilesizes.append(math.log(int(data[4])))
			query_profile = data[2]
			corpus_profile = data[5]
	infile.close()
	return query_profile_sizes, corpus_profile_sizes, scores


def query_parse_results(size):
	scorefile = open("Scores_Sizes.txt", 'w')
	infile = open("Scores.tsv")
	scorefile.write("Query Profile\tQuery Profile Size\tQuery Name\tCorpus Profile\tCorpus Profile Size\tCorpus Profile Name\tOverall Similarity\tURI\n")
	for line in infile:
		if "corpusprofile_label" not in line:
			uri, score, query_profile, query_profile_label, corpus_profile, corpus_profile_label = line.strip().replace("\"","").replace("^^<http://www.w3.org/2001/XMLSchema#string>","").replace("^^<http://www.w3.org/2001/XMLSchema#double>","").replace("<","").replace(">","").split("\t")
			scorefile.write(query_profile + "\t" + str(size[query_profile]) + "\t" + query_profile_label + "\t" + corpus_profile + "\t" + str(size[corpus_profile]) + "\t" + corpus_profile_label + "\t" + str(score) + "\t" + uri + "\n")
	scorefile.close()

	
def studentize(results):
	print ("Doing studentization")
	influence = results.get_influence()
	studentizedresiduals = influence.get_resid_studentized_external()
	return studentizedresiduals


def compute_expect_scores(studentizedresiduals,size_of_corpus):
	print "Computing p-values"
	outfile = open("SemanticSimilarityResults.tsv",'w')
	ranks = open("RankStatistics.txt",'w')
	ranks.write("URI\tStudentized Residuals\tp-value\tExpect Score\n")
	outfile.write("Query Profile ID\tQuery Profile Name\tCorpus Profile ID\tCorpus Profile Name\tOverall Similarity\tExpect Value\n")
	i=0
	infile=open("Scores_Sizes.txt")
	for line in infile:
		if "Query Profile" not in line:
			query_profile, query_profile_size, query_profile_label, corpus_profile, corpus_profile_size, corpus_profile_label, score, uri = line.strip().split("\t")
			score = float(score)
			residual = studentizedresiduals[i]
			pvalue = 1-math.exp(-math.exp(-residual*math.pi/math.sqrt(6)+0.5772156649))
			expect = pvalue * size_of_corpus
			ranks.write(uri + "\t" + str(studentizedresiduals[i]) + "\t" + str(round(pvalue,2)) + "\t" + str(expect) + "\n")
			outfile.write(query_profile + "\t" + query_profile_label + "\t" + corpus_profile + "\t" + corpus_profile_label + "\t" + str(round(score, 2)) + "\t" + str(expect) + "\n")
			i += 1
	ranks.close()	
	infile.close()	
	outfile.close()


def reg_m(scores, sizes):
	print "Doing regression"
	ones = np.ones(len(sizes[0]))
	X = sm.add_constant(np.column_stack((sizes[0], ones)))
	for ele in sizes[1:]:
		X = sm.add_constant(np.column_stack((ele, X)))
	results = sm.OLS(scores, X).fit()
	return results


def plot_residuals(corpus_profile_sizes, query_profile_sizes, scores, corpus_coeff, query_coeff, constant):
	query_axis = []
	corpus_axis = []
	residuals = []
	count = 0
	mean = np.mean(scores)

	for i in range(0, len(scores)):
		query_size = query_profile_sizes[i]
		corpus_size = corpus_profile_sizes[i]
		actualscore = scores[i]
		corpus_axis.append(corpus_size)
		query_axis.append(query_size)
		predictedscore = constant + (corpus_coeff * corpus_size) + (query_coeff * query_size)	
		residual = actualscore - predictedscore
		residuals.append(residual)
		
	plt.scatter(np.array(query_axis), np.array(residuals))
	plt.xlabel('Log(Query Profile Size)')
	plt.ylabel('Residual')
	plt.savefig('ResidualPlot_QuerySizes.png')

	plt.scatter(np.array(corpus_axis), np.array(residuals))
	plt.xlabel('Log(Corpus Profile Size)')
	plt.ylabel('Residual')
	plt.savefig('ResidualPlot_CorpusSizes.png')
	

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