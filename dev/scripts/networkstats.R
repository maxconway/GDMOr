library(igraph)
graph <- getGraph(sulwin)
adjmat <- (as(graph, 'matrix')>0)
net <- network(adjmat, directed = TRUE, bipartite = 609)
res <- set.vertex.attribute(net, 'type', 'metab', match(geo_s_model@met_id, network.vertex.names(net)))
res <- set.vertex.attribute(net, 'type', 'rxn', match(geo_s_model@react_id, network.vertex.names(net)))
activities <- llply(graph@nodes, function(x){getNodeAttribute(sulwin,x,'activity1')})
res <- set.vertex.attribute(net, 'activity1', activities, match(graph@nodes, network.vertex.names(net)))
res <- set.vertex.attribute(net, 'subsystem', geo_s_rxns$SubSystem, match(geo_s_rxns$Abbreviation, network.vertex.names(net)))

measures <- data.frame(
	betweenness = betweenness(ig),
	closeness_out = closeness(ig, mode='out'),
	closeness_in = closeness(ig, mode='in'),
	closeness_all = closeness(ig, mode='all'),
	closeness_total = closeness(ig, mode='total'),
	degree_out = degree(ig, mode='out'),
	degree_in = degree(ig, mode='in'),
	degree_all = degree(ig, mode='all'),
	degree_total = degree(ig, mode='total')
)

model2igraph <- function(model){
	adjmat <- as.matrix(model@S)
	topright <- adjmat>0
	topleft <- matrix(0, nrow=model@met_num, ncol=model@met_num, dimnames=list(model@met_id, model@met_id))
	bottomright <- matrix(0, nrow=model@react_num, ncol=model@react_num, dimnames=list(model@react_id, model@react_id))
	bottomleft <- t(adjmat<0)
	squaremat <- rbind(
		cbind(topleft, topright),
		cbind(bottomleft, bottomright)
		)
	colnames(squaremat) <- c(model@met_id, model@react_id)
	rownames(squaremat) <- c(model@met_id, model@react_id)
	graph <- graph.adjacency(squaremat, mode='directed')
	return(graph)
}

netstat <- function(ig){
	measures <- data.frame(
		name = V(ig)$name,
		betweenness = betweenness(ig),
		closeness_out = closeness(ig, mode='out'),
		closeness_in = closeness(ig, mode='in'),
		closeness_all = closeness(ig, mode='all'),
		closeness_total = closeness(ig, mode='total'),
		degree_out = degree(ig, mode='out'),
		degree_in = degree(ig, mode='in'),
		degree_all = degree(ig, mode='all'),
		degree_total = degree(ig, mode='total')
	)
	return(measures)
}

measuresBySubsystem <- function(model, reactions){
	graph <- model2igraph(model)
	measures <- netstat(graph)
	measuresBySubsystem <- merge(
		x=reactions, y=measures,
		by.x='Abbreviation', by.y='name'
		)
}

rbind(
	measuresBySubsystem(geo_s_model, geo_s_rxns) %>% mutate(species = 'G. Sulfurreducens'),
	measuresBySubsystem(geo_m_model, geo_m_rxns) %>% mutate(species = 'G. Metallireducens')
	) %>%
# 	group_by(species, SubSystem) %>%
# 	summarize(
# 		betweenness = mean(betweenness),
# 		closeness_in = mean(closeness_in), 
# 		closeness_out = mean(closeness_out),
# 		closeness_all = mean(closeness_all)
# 		) %>%
	filter(species=='G. Sulfurreducens') %>%
	ggplot(aes(x=SubSystem, y=betweenness)) + geom_boxplot() + coord_flip() + facet_grid(species~.)
