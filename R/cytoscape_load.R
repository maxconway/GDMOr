#' Loads a metabolic model into cytoscape, and sets up visual mappings
#' 
#' @import RCytoscape
#' @export
cytoscape_load <- function(model){
	# make adjacency matrix
	a <- as.matrix(model@S)
	colnames(a) <- model@react_id
	rownames(a) <- model@met_id
	adj <- rbind(cbind(matrix(0, 
														nrow = nrow(t(a)), 
														ncol = nrow(t(a)), 
														dimnames = list(rownames(t(a)), rownames(t(a)))
	),
	t(a)*(t(a)>0)
	),
	cbind(-a*(a<0),
				matrix(0, 
							 nrow = nrow(a), 
							 ncol = nrow(a), 
							 dimnames = list(rownames(a), rownames(a))
				)
	)
	)
	
	# create graph from adjacency matrix
	graph <- graphAM(adjMat = adj, 
									 edgemode = 'directed',
									 values = list(stoich = -50)
	)
	
	graph <- as(graph,class(graphNEL()))
	
	# initialize graph attributes
	graph <- initEdgeAttribute(graph=graph, 
														 attribute.name='stoich', 
														 attribute.type='numeric', 
														 default.value='-50'
	)
	
	graph <- initNodeAttribute(graph=graph, 
														 attribute.name='type', 
														 attribute.type='char', 
														 default.value='error'
	)
	
	graph <- initNodeAttribute(graph=graph, 
														 attribute.name='label', 
														 attribute.type='char', 
														 default.value='error'
	)
	
	graph <- initNodeAttribute(graph=graph, 
														 attribute.name='activity', 
														 attribute.type='numeric', 
														 default.value='1'
	)
	
	nodeData(graph, model@met_id, 'type') <- 'metabolite'
	nodeData(graph, model@react_id, 'type') <- 'reaction'
	nodeData(graph, model@met_id, 'label') <- model@met_id
	nodeData(graph, model@react_id, 'label') <- model@react_id
	
	# show window
	window <- new.CytoscapeWindow(title = model@mod_id,
																graph = graph
	)
	
	displayGraph(window)
	
	# set shapes
	setNodeShapeRule(obj = window,
									 node.attribute.name = 'type',
									 attribute.values = c('metabolite', 'reaction'),
									 node.shapes = c('round_rect', 'diamond'),
									 default.shape = 'ellipse'
	)
	
	setNodeColorRule(obj = window,
									 node.attribute.name = 'type',
									 mode = 'lookup',
									 control.points = c('metabolite', 'reaction'),
									 colors = c('blue', 'red')
	)
	
	setEdgeColorRule(window, 
									 edge.attribute.name='interaction', 
									 control.points='unspecified', 
									 colors=grey(0), 
									 mode='lookup'
	)
	
	setEdgeOpacityRule(window,
										 edge.attribute.name='stoich',
										 control.points=c(0,1,5),
										 opacities = c(0,127,255),
										 mode = 'interpolate'
	)
	
	window@graph <- graph
	# 	displayGraph(window)
	redraw(window)
	layoutNetwork(window, layout.name = 'force-directed')	
	return(window)
}