# library (RCytoscape)
# cy <- CytoscapeConnection ()
# 
# met <- existing.CytoscapeWindow('geo_m_react.edges')
# sul <- existing.CytoscapeWindow('geo_s_react.edges')
# 
# for (cw in c(met,sul)) {
# 	nodes <- getAllNodes(cw)
# 	edges <- getAllEdges(cw)
# 
# 	selectNodes(cw,nodes,preserve.current.selection=F)
# 	selectEdges(cw,edges,preserve.current.selection=F)
# 	
# 	temp <- createWindowFromSelection(cw,temp,return.graph)
# }