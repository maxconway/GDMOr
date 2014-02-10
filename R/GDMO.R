GDMO <- function(pop, gen, startingpoint, evaluate){
	# Performs GDMO
	#
	# Args:
	#  pop: int, the population to be used throughout
	#  gen: in, the number of generations to run for
	#  startingpoint: a list with a binary element genotype
	#  evaluate: and evaluation function
	#
	# Returns:
	#  a list of pareto optimal solutions, each member 
	#  being a list with elements phenotype and genotype
	parents <- startingpoint
	currentgen = 0
	
	genbar <- txtProgressBar(min = 0, max = gen, style=3)
	while(currentgen <= gen){
		children <- reproduce(parents = parents, pop = pop)
		population <- nondomsort(c(mature(children, evaluate), parents))
		parents <- select(population, maxpop = pop)
		currentgen <- currentgen + 1
		setTxtProgressBar(genbar, currentgen)
	}
	
	return(parents)
}