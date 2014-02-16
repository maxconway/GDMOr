Normalize <- function(x){
	# called internally
	result <- sign(x) * log10(abs(x));
	result[is.nan(result)] <- 0;
	return(result);
}

ShowFluxes <- function(cytoscapeWindow, mod, chromosomes, reactions){
	require(gplots)
	require(sybil)
	# Shows the normalized fluxes in cytoscape.
	# Requires that the graph is already present, and cytoscape attached.
	#
	# Args:
	#   cytoscapeWindow: the cytoscape window which is to be used
	#   mod: the sybil model
	#   chromosomes: the chromosomes that are to be displayed, in order
	#   reactions: reaction table containing columns 'Abbreviation' and 'GeneAssociation'
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