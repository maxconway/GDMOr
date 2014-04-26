#' Sorts and cuts population
#'
#' @param population the whole population datasctructure
#' @param maxpop int, the maximum total population
#'
#' @return A subset of the elements of population, ordered by rank and crowding distance
select <- function(population, maxpop){
	head(population[order(
		sapply(population, function(x){x$front}),
		-sapply(population, function(x){x$crowding}),
		runif(length(population))
		)],
			 n = maxpop)
}