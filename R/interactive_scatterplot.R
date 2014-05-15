#' plot reaction activities in cytoscape
#' 
#' given a reaction activity vector, plot these to cytoscape
#' 
#' @param model a sybil model with valid \code{react_id}s
#' @param activities a named vector of reaction activities, with names from  \code{model@react_ids}
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
#' @param expressions a vector of gene expression values, length equal to \code{length(model@allGenes)}
#' @param cw a cytoscape window
#' @param attribute the cytoscape attribute to be set
#' 
#' @return silently returns the activities of the reactions as a named vector, but mainly used for the side effect of setting the 'activity' attribute in cytoscape.
#' 
#' @export
show_gene_expression <- function(model, expressions, cw, attribute='activity'){
	rxnact <- gene2rxn(model, expressions)
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
#' @param model a sybil model
#' @param cw a cytoscape window
#' @param attribute the cytoscape attribute to be set
#' 
#' @export
#' 
#' @examples
#' show_gene_expression(model, dataset[point_selector(dataset,'phenotype.one','phenotype.two', cw),grep('genotype',colnames(dataset))], cw)
point_selector <- function(dataset, x, y, model, cw, attribute='activity'){
	plot(x=dataset[[x]], y=dataset[[y]], xlab = x, ylab = y, cex=0.5)
	ind <- identify(x=dataset[[x]], y=dataset[[y]], n=1, plot=FALSE)
	
	return(ind)
	
}

#' @title Select a box from a scatterplot
#'   
#' @description Creates a scatter plot, then allows the user to select two
#'   points to define a rectangle of selected points.
#'   
#' @param dataset a data frame
#' @param x,y column names from the data frame to be plotted
#' 
#' @return a logical vector indicating if the points are in the box.
#'   
#' @import ggplot2 ggmap
# box_selector <- function(dataset, x, y){
# 	
# 	ggplot(dataset, aes_string(x=x, y=y))
# 	
# 	print(bg + geom_point())
# 	
# 	print('click twice on the graph to define a rectangle containing the points of interest')
# 	pts <- gglocator(n=2, message=TRUE)
# 	
# 	selected <- (dataset[[x]]<=max(pts[[x]]) &
# 							 	dataset[[x]]>=min(pts[[x]]) & 
# 							 	dataset[[y]]<=max(pts[[y]]) &
# 							 	dataset[[y]]>=min(pts[[y]]))
# 	
# 	print(bg + geom_rect(aes(xmax=max(pts[[x]]),
# 										 xmin=min(pts[[x]]),
# 										 ymax=max(pts[[y]]),
# 										 ymin=min(pts[[y]])), alpha=0.9) + geom_point(aes(colour=selected)))
# 	
# 	return(selected)
# }

#' @title divid a scatter plot by a line
#'   
#' @description Creates a scatter plot, then allows the user to select two
#'   points to define a line.
#'   
#' @param dataset a data frame
#' @param x,y column names from the data frame to be plotted
#' 
#' @return a logical vector indicating if the points are 
#'   
#' @import ggplot2 ggmap
# line_selector <- function(dataset, x, y){
# 	
# 	bg <- ggplot(dataset, aes_string(x=x, y=y))
# 	
# 	print(bg + geom_point())
# 	
# 	print('click twice on the graph to define a line dividing the points')
# 	points <- gglocator(n=2)
# 	
# 	vec <- points[2,] - points[1,]
# 	slope = vec[,y] / vec[,x]
# 	intercept = points[1,y] - slope*points[1,x]
# 	
# 	datavecs <- dataset[,c(x,y)] - points[rep.int(1,nrow(dataset)),]
# 	side <- vec[,x]*datavecs[,y] > vec[,y]*datavecs[,x]
# 	
# 	print(bg + geom_abline(slope=slope, intercept=intercept, linetype=2) + geom_point(aes(colour=side)))
# 	return(side)
# }

