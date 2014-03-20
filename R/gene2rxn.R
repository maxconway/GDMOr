#' @title gene to reaction lookup
#'   
#' @description Takes a sybil models and a character vector of active genes, and
#' finds active reactions from this.
#' 
#' @param model a sybil model
#' @param genes a character vector genes
#' 
#' @return a character vector of reactions
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
	stopifnot(genes %in% model@allGenes)
	
	env <- new.env(parent = baseenv())
	assign('x', genes, env)
	selectedrxns_bool <- aaply(mod@gprRules,1,function(rule){
		res <- eval(expr=parse(text=rule),envir=env)
		ifelse(is.null(res),FALSE,res)
	})
	selectedrxns <- mod@react_id[selectedrxns_bool]
}