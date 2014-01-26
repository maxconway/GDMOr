reproduce <- function(parents){
	lapply(parents[sample.int(n = length(parents))], 
	# produces a new population by mutating the parent or the required size
	#
	# Args:
	#  parents: A list of lists, with inner lists having a binary 'genotype' element.
	#  pop: an int, population size
	# 
	# Returns:
	#  A list of lists of length pop, with inner lists having a 'genotype' element.
				 function(x){
				 	genotype <- x$genotype
				 	flip <- sample.int(length(genotype),1)
				 	genotype[flip] <- !genotype[flip]
				 	list(genotype = genotype)
				 })
}