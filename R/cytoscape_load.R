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
	
	nodeData(graph, model@met_id, 'type') <- 'metabolite'
	nodeData(graph, model@react_id, 'type') <- 'reaction'
	
	# show window
	window <- new.CytoscapeWindow(title = model@mod_name,
																graph = graph
	)
	
	# set shapes
	setNodeShapeRule(obj = window,
									 node.attribute.name = 'type',
									 attribute.values = c('metabolite', 'reaction'),
									 node.shapes = c('round_rect', 'diamond'),
									 default.shape = 'ellipse'
									 )
	
	window@graph <- graph
	displayGraph(window)
	layoutNetwork(window, layout.name = 'force-directed')	
}