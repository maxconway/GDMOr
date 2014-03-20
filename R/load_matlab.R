#' @title Load a matlab population file
#' 
#' @description
#' Loads a matlab data file (or files) representing a population
#' 
#' @param files a list of files, with each row representing a member of the population, and columns representing: \enumerate{
#'  \item genes in the population (either as a boolean presence, or an expression value)
#'  \item the phenotypes
#'  \item rank
#'  \item crowding distance
#' }
#' @param genotype_names a vector of names for the genotype columns
#' @param phenotype_names a vector of names for the phenotype columns
#' 
#' @import R.matlab plyr
#'
#' @export
load_pop_matlab <- function(files, genotype_names, phenotype_names){
	res <- ldply(files,function(file){
		readMat(file)[[1]]
	})
	colnames(res) <- c(paste0('genotype.',genotype_names),paste0('phenotype.',phenotype_names),'rank','crowding')
	return(res)
}