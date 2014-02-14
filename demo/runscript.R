# Sybil runscript
#  uses sybil to load a model, run GDMO on it, and put it back into a data frame

# Load libraries
library(sybil)
library(sybilSBML)
library(glpkAPI)
library(plyr)

# Load data
mp <- system.file(package = "sybil", "extdata")
Ec_core <- readTSVmod(prefix = "Ec_core", fpath = mp, quoteChar = "\"")
targetFluxes <- c('Biomass_Ecoli_core_w_GAM', 'EX_ac(e)')

# Create starting point
start <- as.list(rep(TRUE, times = length(allGenes(Ec_core))))
names(start) <- allGenes(Ec_core)
ancestors <- list(list(genotype = start))

# create evaluation function
evaluate <- function(genotype){
	# avoid an error with no knockouts
	# sybil should really deal with this
	genes <- if(suppressWarnings(all(genotype))){
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
	phenotype <- solution$fluxes[match(targetFluxes,react_id(Ec_core))]
}

reslist <- GDMO(300, 300, ancestors, evaluate)

resdf <- ldply(.data = reslist, .progress = 'text', .fun = function(individual){
	data.frame(
		genotype = individual$genotype,
phenotype = {names(individual$phenotype) <- targetFluxes; as.list(individual$phenotype)},
dom = individual$dom,
crowding = individual$crowding,
kos = individual$kos
	)
})

