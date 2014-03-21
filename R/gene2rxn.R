#' @title gene to reaction lookup
#'   
#' @description Takes a sybil models and a character vector of active genes, and
#' finds active reactions from this.
#' 
#' @param model a sybil model
#' @param genes a vector vector of gene presence or expression
#' 
#' 
#' @return a vector of reaction presence of activity
#'   
#' @details Works by creating a new environment and evaluating gpr rules
#' computationally. Could be a security concern, so check what you're choosing
#' to run.
#' 
#' @import sybil
#' 
#' @export
gene2rxn <- function(model, genes){
	# check arguments
	if(length(genes) != length(model@allGenes)){
		stop('requires a vector of gene presence. Try model@allGenes %in% genes')
	}
	
	env <- new.env(parent = baseenv())
	assign('x', genes, env)
	
	if(is.numeric(genes)){
		assign('&', function(x,y){min(x,y)}, env)
		assign('|', function(x,y){max(x,y)}, env)
	}
	
	rxnsactivity <- laply(model@gprRules,function(rule){
		res <- eval(expr=parse(text=rule),envir=env)
		ifelse(is.null(res),1,as.numeric(res))
	})
	return(rxnsactivity)
}