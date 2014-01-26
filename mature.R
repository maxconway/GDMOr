mature <- function(children,evaluate){
	children$phenotype <- evaluate(children$genotype)
	# apply the evaluation function
	# 
	# Args: 
	#  children: a list of lists, each inner element having an element genotype
	#  evaluate: a function that takes a binary vector, and returns a real vector
	#
	# Returns:
	# A list of lists similar to children, but with a phenotype element added to each inner element
	return(children)
}