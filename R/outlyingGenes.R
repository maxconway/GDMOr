#' Produces graphs showing genes with unusually high correlations with position in the pareto front.
#' 
#' @param dataset a dataframe with some columns prefixed by \code{genotype.},
#'   and some by \code{phenotype.}
#' @param lowerlimit lower bound to mark outlying genes, in standard deviations
#' @param upperlimit upper bound to mark outlying genes, in standard deviations
#' @param genes_subsystems a data.frame with columns \code{name} and \code{SubSystem}. If this is supplied, a further graph will be produced
#' 
#' @return names of outlying genes
#' 
#' @import ggplot2 grid stringr kernlab dplyr
#' 
#' @export
outlyingGenes <- function(dataset, lowerlimit=-2, upperlimit=2, genes_subsystems){
	genes_subsystems_present <- !missing(genes_subsystems)
	
	dataset$pos <- as.numeric(kpca(~.,
																 dataset[,grep(pattern='phenotype.*',setdiff(colnames(dataset),'phenotype.kos'))], 
																 kernel = polydot(degree=5), 
																 features=1
	)@rotated)
	
	a <- data.frame(name = str_replace_all(grep(pattern='genotype.*',colnames(dataset),value=T),fixed('genotype.'),''),
									correlation = cor(dataset)[,'pos'][grep(pattern='genotype.*',colnames(dataset),value=T)],
									expression = colMeans(dataset)[grep(pattern='genotype.*',colnames(dataset),value=T)]
	)
	#   plot(density(na.omit(a$correlation)))
	
	if(!genes_subsystems_present){
		a$subSystem <- 'black'
	}else{
		a$subSystem <- factor(genes_subsystems[match(a$name, genes_subsystems$genes), 'SubSystem'])
	}
	
	a.norm <- a[abs(a$correlation-mean(a$correlation,na.rm=T))<3*sd(a$correlation,na.rm=T),]
	boundaries <- c(lower = lowerlimit*sd(a.norm$correlation,na.rm=T),
									upper = upperlimit*sd(a.norm$correlation,na.rm=T)
	)
	
	bothplots <- function(plot){
		return(plot
					 +xlim(-1,1)
					 +theme(panel.grid.major.y = element_blank(),
					 			 panel.grid.minor.y = element_blank(),
					 			 axis.ticks.y = element_blank(),
					 			 axis.text.y = element_blank(),
					 			 legend.position="none"
					 			 
					 )
					 +geom_vline(xintercept=boundaries,linetype='dotted')
		)
	}
	
	densityPlot <- bothplots(ggplot(a, aes(x = correlation))
													 +stat_density(alpha = 0, colour = 'black', size = 1)
													 +scale_y_continuous(name = "density")
													 +theme(axis.text.x = element_blank(),
													 			 axis.title.x = element_blank()
													 )
	)
	
	distributionPlot <- ggplot(a[abs(a$correlation)!=0,]) +
		geom_boxplot(aes(x=subSystem, y=correlation, colour=subSystem, fill=subSystem)) +
		ylim(-1,1) +
		theme(
			legend.position="none",
			axis.ticks.y = element_blank(),
			axis.text.y = element_blank()
			) +
		geom_hline(yintercept=boundaries,linetype='dotted') +
		coord_flip()
	
	# 	if(any(findInterval(a$correlation,boundaries)!=1)){
	# 		distributionPlot <- distributionPlot + geom_text(data = a[findInterval(a$correlation,boundaries)!=1,],
	# 																										 aes(x = correlation,
	# 																										 		size = 2.5,
	# 																										 		y = name,
	# 																										 		label = round(expression,3),
	# 																										 		hjust = 0.5+0.5*-sign(correlation)
	# 																										 ),
	# 																										 show_guide = FALSE
	# 		)
	# 	}
	
	if(genes_subsystems_present){
		subsystemPlot <- a %>% 
			group_by(subSystem) %>% 
			dplyr::summarise(`mean expression` = mean(expression)) %>% 
			ggplot(aes(x=subSystem, y=`mean expression`, fill = subSystem)) +
			geom_hline(yintercept = 1, colour='dimgrey') +
			geom_bar(stat='identity') + scale_fill_discrete() +
			coord_flip() + theme(legend.position="none", 
													 axis.text.y=element_text(angle=40, colour='gray30', size=rel(0.75)),
													 axis.ticks.y = element_blank(),
													 axis.title.y = element_blank()
			)
	}
	
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(5,5)))
	if(genes_subsystems_present){
		plot(subsystemPlot,vp=viewport(layout.pos.row=1:5, layout.pos.col=1:2))
		verticaldivide <- 3
	}else{
		verticaldivide <- 1
	}
	print(densityPlot,vp=viewport(layout.pos.row=1,layout.pos.col=verticaldivide:5))
	print(distributionPlot,vp=viewport(layout.pos.row=2:5,layout.pos.col=verticaldivide:5))
	
	return(na.omit(a[findInterval(a$correlation,boundaries)!=1,]))
}