Normalize <- function(x){
	# called internally
	result <- sign(x) * log10(abs(x));
	result[is.nan(result)] <- 0;
	return(result);
}

#' Shows the normalized fluxes in cytoscape.
#' 
#' Requires that the graph is already present, and cytoscape attached.
#' Defunct until change to new chromosome datastructure.
#'
#' @param cytoscapeWindow the cytoscape window which is to be used
#' @param mod the sybil model
#' @param chromosomes the chromosomes that are to be displayed, in order
#' @param reactions reaction table containing columns \code{Abbreviation} and \code{GeneAssociation}
#' 
#' @import gplots sybil
ShowFluxes <- function(cytoscapeWindow, mod, chromosomes, reactions){
	setNodeColorRule(cytoscapeWindow,
									 node.attribute.name='flux',
									 control.points=c(-3.3,0,3.3),
									 colors=col2hex(c('black','red','white','green','black')),
									 mode='interpolate'
	)
	
	
	changed <- T;
for(i in 1:nrow(chromosomes)){
	chrom<-chromosomes[i,]
	sol <- optimizeProb(object=mod,gene=names(chrom)[chrom==T],lb=0,ub=0)
	fluxes <- as.vector(fluxes(sol))
	if(i>1){
		changed <- old != fluxes
		#		print(summary(old-fluxes))
	}
	old <- fluxes
	setNodeAttributesDirect(cytoscapeWindow,'flux','numeric',as.character(reactions[,'abbreviation'])[changed],Normalize(fluxes)[changed])
	redraw(cytoscapeWindow)
	msg(cytoscapeWindow,paste('timestep',i))
}
}