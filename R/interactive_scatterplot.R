#' plot reaction activities in cytoscape
#' 
#' given a reaction activity vector, plot these to cytoscape
#' 
#' @param model a sybil model with valid \code{react_id}s
#' @param activities a named vector of reaction activities, with names from \code{model@@react_ids}
#' @param cw a cytoscape window
#' @param attribute the cytoscape attribute to be set
#' 
#' @return silently returns the activities of the reactions as a named vector, but mainly used for the side effect of setting the 'activity' attribute in cytoscape.
#' 
#' @export
show_reaction_activity <- function(model, activities, cw, attribute='activity'){
	graph <- initNodeAttribute(graph=getGraph(cw), 
														 attribute.name=attribute, 
														 attribute.type='numeric', 
														 default.value='1'
	)
	displayGraph(cw)
	setNodeAttributesDirect(obj=cw, 
													attribute.name=attribute, 
													attribute.type='numeric', 
													node.names=model@react_id, 
													values=activities[model@react_id]
	)
	invisible(activities)
}

#' Plot gene expressions in cytoscape
#' 
#' Given a gene expressions, plot the associated reaction activities to cytoscape.
#' This function is a wrapper around \code{show_reaction_activities} and \code{gene2rxn}
#' 
#' @param model a sybil model with valid \code{react_id}s
#' @param expressions a vector of gene expression values, length equal to \code{length(model@@allGenes)}
#' @param cw a cytoscape window
#' @param attribute the cytoscape attribute to be set
#' 
#' @return silently returns the activities of the reactions as a named vector, but mainly used for the side effect of setting the 'activity' attribute in cytoscape.
#' 
#' @export
show_gene_expression <- function(model, expressions, cw, attribute='activity'){
	rxnact <- gene2rxn(expressions, model)
	names(rxnact) <- model@react_id
	
	show_reaction_activity(model, rxnact, cw, attribute)
}

#' @title Select a point from a scatterplot
#'   
#' @description Creates a scatter plot, and allows the user to select a point 
#'   from it. Returns the index of the point in the dataset. See examples for typical usage
#'   
#' @param dataset a data frame
#' @param x,y column names from the data frame to be plotted
#' 
#' @export
#' 
#' @examples
#' point_selector(mtcars, 'hp', 'mpg')
point_selector <- function(dataset, x, y){
	plot(x=dataset[[x]], y=dataset[[y]], xlab = x, ylab = y)
	ind <- identify(x=dataset[[x]], y=dataset[[y]], n=1, plot=FALSE)
	
	return(ind)
	
}
