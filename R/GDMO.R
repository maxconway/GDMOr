#' Performs GDMO
#' 
#' @param population int, the population to be used.
#' @param generations: int, the number of generations to run for
#' @param startingpoint: a list with a binary element genotype
#' @param evaluate: and evaluation function
#'   
#' @return a list of pareto optimal solutions, each member being a list with
#'   elements phenotype and genotype
#'   
#' @export
#' @import lubridate
GDMO <- function(population, generations, startingpoint, evaluate, savefile = 'checkpoint.RData'){
	parents <- startingpoint
	currentgen <- 1
	
	genbar <- txtProgressBar(min = 0, max = generations, style=2)
	starttime <- now()
#	tryCatch(
		while(currentgen <= generations){
			children <- reproduce(parents = parents, pop = population)
			adults <- nondomsort(c(mature(children, evaluate), parents))
			parents <- select(adults, maxpop = population)
			currentgen <- currentgen + 1
			if((now() - starttime) > new_difftime(seconds = 30) && currentgen%%10==0){
				save(parents, file = savefile)
			}
			setTxtProgressBar(genbar, currentgen)
			print(now() + (now()-starttime)*(generations/currentgen-1))
		}#,
#		finally = return(parents)
#	)
}

GDMO_sybil <- function(model, population=10, generations=10, targets, targetdirections, savefile = 'checkpoint.RData'){
	
	# Create starting point
	start_genotype <- as.list(rep(TRUE, times = length(allGenes(model))))
	start_phenotype <- as.list(rep(NULL, times = length(targets)+1))
	names(start_genotype) <- allGenes(model)
	names(start_phenotype) <- c(targets,'kos') 
	ancestors <- list(list(genotype = start_genotype, phenotype = start_phenotype))
	
	# set up memoization
	memo <- list()
	
	# create evaluation function
	evaluate <- function(genotype){
		
		# Check memoization
		phenotype <- memo[[digest(genotype)]]
		if(!is.null(phenotype)){
			return(phenotype)
		}
		
		
		# avoid an error with no knockouts
		# sybil should really deal with this
		genes <- if(suppressWarnings(all(genotype))){
			NULL
		} else{
			names(genotype)[genotype==FALSE]
		}
		# call sybil to compute the fluxes
		solution <- optimizeProb(object = Ec_core, 
														 gene = genes,
														 lb = 0,
														 ub = 0,
														 retOptSol = FALSE)
		phenotype <- c(solution$fluxes[match(targets,react_id(Ec_core))]*-targetdirections, sum(genotype==FALSE))
		
		# memoize
		memo[[digest(genotype)]] <<- phenotype
		return(phenotype)
	}
	
	return(GDMO(population, generations, ancestors, evaluate, savefile))
	
}