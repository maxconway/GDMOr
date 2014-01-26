reproduce <- function(parents){
	lapply(parents[sample.int(n = length(parents))], 
				 function(x){
				 	genotype <- x$genotype
				 	flip <- sample.int(length(genotype),1)
				 	genotype[flip] <- !genotype[flip]
				 	list(genotype = genotype)
				 })
}