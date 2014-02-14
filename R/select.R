select <- function(population, maxpop){
	# Sorts and cuts population
	#
	# Args:
	#  population: the whole population datasctructure
	#  maxpop: int, the maximum total population
	#
	# Returns:
	#  A subset of the elements of population, ordered by rank and crowding distance
	head(population[order(
		sapply(population, function(x){x$dom}),
		-sapply(population, function(x){x$crowding}),
		-sapply(population, function(x){x$kos}),
		runif(length(population))
		)],
			 n = maxpop)
}