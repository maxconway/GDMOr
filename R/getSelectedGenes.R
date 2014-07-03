#' GetSelectedRxns
#' 
#' Retrieve the reaction ids associated with nodes selected in cytoscape
#' 
#' @param cw a cytoscape window
#' @param model a sybil model
#' 
#' @return a character vector of reaction ids
#' 
#' @family GetSelected*
#' 
#' @import RCytoscape sybil
#' @export
GetSelectedRxns <- function(cw, model){
	ids <- getSelectedNodes(cw)
	rxns <- model@react_id %in% ids %.% setdiff('') # Using %.% to create an ad-hoc infix operator
}

#' GetSelectedRxns
#' 
#' Retrieve the reaction ids associated with nodes selected in cytoscape
#' 
#' @param cw a cytoscape window
#' @param model a sybil model
#' 
#' @return a character vector of gene ids
#' 
#' @family GetSelected*
#' 
#' @import RCytoscape sybil
#' @export
getSelectedGenes <- function(cw, model){
	genes <- unlist(model@genes[GetSelectedRxns(cw, model)]) %.% setdiff('') # Using %.% to create an ad-hoc infix operator 
}