nondomsort <- function(population){
	# Adds a property specifying the number of individuals that dominate each individual
	#
	# Args:
	#  population: a list of lists, each inner element must have a vector element 'phenotype'
	#
	# Returns:
	#  a list of lists like population, but with a real 'dom' appended to each inner element
	
	# Would probably be faster to us a vectorized version
	
	phenotypes <- ldply(reslist,function(x){x$phenotype})
	phenotypes$front <- 0
	front <- 0
	while(any(phenotypes$front==front)){
		phenotypes[phenotypes$front==front,'front'] <- front + 
			sapply(phenotypes[phenotypes$front==front,],
						 function(member){any(sapply(phenotypes[phenotypes$front==front,], function(othermember){
						 	all(othermember$phenotype >= member$phenotype) & any(othermember$phenotype > member$phenotype)
						 }))}
			)
	}
	
	population <- lapply(1:length(population), function(x){
		member <- population[[x]]
		othermembers <- population[-x]		
		
		# placeholder
		member$front <- phenotypes[x,'front']
		
		# placeholder
		member$crowding <- min(sapply(othermembers, function(othermember){
			sqrt(sum((othermember$phenotype-member$phenotype)^2))
		}))
		
		member$kos <- sum(member$genotype==FALSE)
		
		return(member)
	})
}