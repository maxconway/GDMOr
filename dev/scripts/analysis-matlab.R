# Analysis script
library('R.matlab')
library('sybil')
library('RCytoscape')
library('plyr')
library('dplyr')
library('ggplot2')
library('ggvis')
library('ggmap')
library('devtools')
load_all()


chrom <- load_pop_matlab(files=list.files(path='dev/Matlab/expFBA/solutions',pattern='solution1\\d\\d0.mat',full.names=TRUE),
												 genotype_names=unlist(readMat('dev/Matlab/expFBA/geni.mat')$geni),
												 phenotype_names=c('biomass','atp'))
chrom <- chrom[sample.int(n=nrow(chrom),size=500),]

load(file='./dev/raw/PNASMS2013-07797_DatasetS2_mutli_ecoli_55_models.RData')

model <- models$iJO1366
cw <- cytoscape_load(model=models$iJO1366)

point_selector(dataset=chrom, x='phenotype.biomass', y='phenotype.atp', model=model, cw=cw)



ping()
