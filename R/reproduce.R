#' Produces a new population by mutating the parent or the required size
#'
#' @param parents A list of lists, with inner lists having a binary 'genotype' element.
#' @param pop an int, population size
#' 
#' @param A list of lists of length pop, with inner lists having a 'genotype' element.
reproduce <- function(parents, pop){
	lapply(parents[sample.int(n = length(parents), size = pop, replace = TRUE)], 
				 function(x){
				 	genotype <- x$genotype
				 	flip <- sample.int(length(genotype),1)
				 	genotype[flip] <- genotype[flip]==FALSE
				 	list(genotype = genotype)
				 })
}