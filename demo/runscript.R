# Sybil runscript
#  uses sybil to load a model, run GDMO on it, and put it back into a data frame

# Load libraries
library(sybil)
library(sybilSBML)
library(glpkAPI)

# Load data
mp <- system.file(package = "sybil", "extdata")
Ec_core <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")

# Create starting point
start <- as.list(rep(TRUE, times = length(allGenes(Ec_core))))
names(start) <- allGenes(Ec_core)
ancestors <- list(list(genotype = start))

# create evaluation function
evaluate <- function(genotype){
	# avoid an error with no knockouts
	# sybil should really deal with this
	genes <- if(all(genotype)){
		NULL
	} else{
		names(genotype)[genotype==FALSE]
	}
	# call sybil to computer the fluxes
	solution <- optimizeProb(object = Ec_core, 
													 gene = genes,
													 lb = 0,
													 ub = 0,
													 retOptSol = FALSE)
	solution$fluxes[match(c('Biomass_Ecoli_core_w_GAM', 'EX_ac(e)'),react_id(Ec_core))]
}

GDMO(100, 100, ancestors, evaluate)

