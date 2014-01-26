nondomsort <- function(population){
	population <- lapply(population, function(member){
		member$dom <- sum(lapply(population, function(othermember){
			all(othermember$phenotype >= member$phenotype) & any(othermember$phenotype > member$phenotype)
		}))
	})
}