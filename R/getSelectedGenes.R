#' Retrieve reactions from Cytoscape
#' 
#' Retrieve the reaction ids associated with nodes selected in cytoscape
#' 
#' @param cw a cytoscape window
#' @param model a sybil model
#' 
#' @return a character vector of reaction ids
#' 
#' @family GetSelected
#' 
#' @import RCytoscape sybil
#' @export
get_selected_rxns <- function(cw, model){
	ids <- getSelectedNodes(cw)
	rxns <- intersect(model@react_id, ids) # Using %.% to create an ad-hoc infix operator
}

#' Retrieve genes from Cytoscape
#' 
#' Retrieve the gene ids associated with nodes selected in cytoscape
#' 
#' @param cw a cytoscape window
#' @param model a sybil model
#' 
#' @return a character vector of gene ids
#' 
#' @family GetSelected
#' 
#' @import RCytoscape sybil
#' @export
get_selected_genes <- function(cw, model){
	genes <- setdiff(unlist(model@genes[model@react_id %in% getSelectedRxns(cw, model)]), '') # Using %.% to create an ad-hoc infix operator 
}
