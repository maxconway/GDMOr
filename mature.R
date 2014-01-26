mature <- function(children,evaluate){
	children$phenotype <- evaluate(children$genotype)
	return(children)
}