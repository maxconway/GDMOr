GDMO <- function(population, generations, startingpoint, evaluate){
	# Performs GDMO
	#
	# Args:
	#  population: int, the population to be used.
	#  generations: int, the number of generations to run for
	#  startingpoint: a list with a binary element genotype
	#  evaluate: and evaluation function
	#
	# Returns:
	#  a list of pareto optimal solutions, each member 
	#  being a list with elements phenotype and genotype
	parents <- startingpoint
	currentgen <- 1
	
	genbar <- txtProgressBar(min = 0, max = generations, style=3)
	starttime <- proc.time()[3]
	tryCatch(
		while(currentgen <= generations){
			children <- reproduce(parents = parents, pop = population)
			adults <- nondomsort(c(mature(children, evaluate), parents))
			parents <- select(adults, maxpop = population)
			currentgen <- currentgen + 1
			if(proc.time()[3] - starttime > 300 && currentgen%%10==0){
				save(parents, file='checkpoint.RData')
			}
			setTxtProgressBar(genbar, currentgen)
		},
		finally = return(parents)
	)
}