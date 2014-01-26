nondomsort <- function(population){
	# Adds a property specifying the number of individuals that dominate each individual
	#
	# Args:
	#  population: a list of lists, each inner element must have a vector element 'phenotype'
	#
	# Returns:
	#  a list of lists like population, but with a real 'dom' appended to each inner element
	population <- lapply(population, function(member){
		member$dom <- sum(sapply(population, function(othermember){
			all(othermember$phenotype >= member$phenotype) & any(othermember$phenotype > member$phenotype)
		}))
		return(member)
	})
}