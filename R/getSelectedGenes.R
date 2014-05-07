#' Find the genes associated with the selected nodes in the cytoscape graph
#' 
getSelectedGenes <- function(model, cw){
	ids <- getSelectedNodes(cw)
	genes <- unlist(model@genes[model@react_id %in% ids]) %.% setdiff('') # Using %.% to create and ad-hoc infix operator 
}