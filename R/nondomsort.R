nondomsort <- function(population){
	# Adds a property specifying the number of individuals that dominate each individual
	#
	# Args:
	#  population: a list of lists, each inner element must have a vector element 'phenotype'
	#
	# Returns:
	#  a list of lists like population, but with a real 'dom' appended to each inner element
	# 
	#' @import plyr
	
	# Would probably be faster to us a vectorized version
	
	phenotypes <- ldply(population,function(x){x$phenotype})
	phenotypes$front <- 0
	front <- 0
	while(any(phenotypes$front==front)){
		phenotypes[phenotypes$front==front,'front'] <- front + 
			aaply(.data = phenotypes[phenotypes$front==front,-ncol(phenotypes)],
						.margins = 1,
						.expand = FALSE,
						.fun = function(member){
							any(aaply(.data = phenotypes[phenotypes$front==front,-ncol(phenotypes)], 
												.margins = 1, 
												.expand = FALSE,
												.fun = function(othermember){
													all(othermember >= member) & any(othermember > member)
												}
							))
						}
			)
		front <- front +1
	}
	
	population <- lapply(1:length(population), function(x){
		member <- population[[x]]
		othermembers <- population[-x]		
		
		member$front <- phenotypes[x,'front']
		
		# placeholder
		member$crowding <- mean(head(n = 2,
																 x = sort(partial = c(1,2), 
																 				 x = aaply(.data = othermembers, 
																 				 					 .margins = 1,
																 				 					 .expand = FALSE,
																 				 					 .fun = function(othermember){
																 				 	sqrt(sum((othermember$phenotype-member$phenotype)^2))
																 				 }
																 				 )
																 )
		)
		)		
		return(member)
	})
}