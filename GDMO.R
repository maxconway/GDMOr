GDMO <- function(pop, gen, startingpoint, evaluate){
	parents <- lapply(1:pop,function(x){startingpoint})
	# Performs GDMO
	#
	# Args:
	#  pop: int, the population to be used throughout
	#  gen: in, the number of generations to run for
	#  startingpoint: a list with a binary element phenotype
	#  evaluate: and evaluation function
	#
	# Returns:
	#  a list of pareto optimal solutions, each member 
	#  being a list with elements phenotype and genotype
	currentgen = 0
	while(currentgen <= gen){
		children <- reproduce(parents = parents)
		population <- nondomsort(list(mature(children, evaluate), parents))
		parents <- select(population, maxpop = pop)
		currentgen <- currentgen + 1
	}
}