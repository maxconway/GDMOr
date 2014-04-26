#' apply the evaluation function
#' 
#' @param children a list of lists, each inner element having an element genotype
#' @param evaluate a function that takes a binary vector, and returns a real vector
#'
#' @return A list of lists similar to children, but with a phenotype element added to each inner element
mature <- function(children,evaluate){
	children <- lapply(children, function(child){
		child$phenotype <- evaluate(child$genotype)
		return(child)
	})
	return(children)
}