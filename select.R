select <- function(population, maxpop){
	head(population[order(
		sapply(population,function(x){x$rank}),
		-sapply(population,function(x){x$crowding}),
		runif(length(population))
		)],
			 n = maxpop)
}