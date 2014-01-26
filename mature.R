mature <- function(children,evaluate){
	# apply the evaluation function
	# 
	# Args: 
	#  children: a list of lists, each inner element having an element genotype
	#  evaluate: a function that takes a binary vector, and returns a real vector
	#
	# Returns:
	# A list of lists similar to children, but with a phenotype element added to each inner element
	children <- lapply(children, function(child){
		child$phenotype <- evaluate(child$genotype)
		return(child)
	})
	return(children)
}