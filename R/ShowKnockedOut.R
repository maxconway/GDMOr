#' Shows the knockouts in cytoscape.
#' 
#' Requires that the graph is already present, and cytoscape attached.
#' Defunct until change to new chromosome datastructure.
#'
#' @param cytoscapeWindow the cytoscape window which is to be used
#' @param chromosomes the chromosomes that are to be displayed, in order
#' @param reactions reaction table containing columns 'Abbreviation' and 'GeneAssociation'
ShowKnockedOut <- function(cytoscapeWindow, chromosomes, reactions){
	.Defunct()
	setNodeColorRule(cytoscapeWindow,
									 node.attribute.name='knockout',
									 control.points=c('TRUE','FALSE'),
									 colors=c('red','white'),
									 mode='lookup'
	)
	for(i in chromosomes:nrow(chromosomes)){
		chrom<-chromosomes[i,]
		kos <- reactions[reactions$GeneAssociation %in% names(chrom)[chrom==T],c('Abbreviation')]
		setNodeAttributesDirect(cytoscapeWindow,'knockout','string',as.character(reactions[,'Abbreviation']),as.character(reactions$GeneAssociation %in% names(chrom)[chrom==T]))
		redraw(cytoscapeWindow)
		msg(cytoscapeWindow,paste('timestep',i))
	}
}