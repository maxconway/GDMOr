require(ggplot2)
require(grid)
#' Produces a pair of graphs showing genes with unusually high correlations with position in the pareto front.
#' 
#' @param dataset a dataframe with some columns prefixed by \code{genotype.},
#'   and some by \code{phenotype.}
#' 
#' @import ggplot2
#' @import grid
#' @import stringr
#' @import kernlab
#' 
#' @export
outlyingGenes <- function(dataset, lowerlimit=-2, upperlimit=2){
	
	dataset$pos <- as.numeric(kpca(~.,
											dataset[,grep(pattern='phenotype.*',setdiff(colnames(dataset),'phenotype.kos'))], 
											kernel = polydot(degree=5), 
											features=1
	)@rotated)
	
	a <- data.frame(name = str_replace_all(grep(pattern='genotype.*',colnames(dataset),value=T),fixed('genotype.'),''),
									correlation = cor(dataset)[,'pos'][grep(pattern='genotype.*',colnames(dataset),value=T)]
	)
	#   plot(density(na.omit(a$correlation)))
	
	a.norm <- a[abs(a$correlation-mean(a$correlation,na.rm=T))<3*sd(a$correlation,na.rm=T),]
	#   lines(density(rnorm(5000,mean(a.norm$correlation,na.rm=T),sd(a.norm$correlation,na.rm=T))),col='blue')
	#   
	#   ggplot(na.omit(a[abs(a$cor)>0,]))+aes(x=name,y=correlation)+geom_bar(stat='identity')+ylim(-1,+1) + theme(axis.text.x = element_blank())
	#   ggplot(na.omit(a[abs(a$correlation)>2*sd(a.norm$correlation,na.rm=T),]))+aes(x=name,y=correlation)+geom_bar(stat='identity')+ylim(-1,+1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
	#   
	bothplots <- function(plot){
		return(plot
					 +xlim(-1,1)
					 +theme(panel.grid.major.y = element_blank(),
					 			 panel.grid.minor.y = element_blank(),
					 			 axis.ticks.y = element_blank(),
					 			 axis.text.y = element_blank()
					 			 
					 )
					 +geom_vline(xintercept=c(upperlimit*sd(a.norm$correlation,na.rm=T),lowerlimit*sd(a.norm$correlation,na.rm=T)),linetype='dotted')
		)
	}
	
	densityPlot <- bothplots(ggplot(a,aes(correlation))
													 +stat_density(aes(y=..count..))
													 +scale_y_continuous(name="density")
													 +theme(axis.text.x = element_blank(),
													 			 axis.title.x = element_blank())
	)
	
	distributionPlot <- bothplots(ggplot(a[abs(a$correlation)!=0,],aes(x=correlation,y=name))
																+geom_point()
																+geom_text(data = a[a$correlation>upperlimit*sd(a.norm$correlation,na.rm=T) | a$correlation<lowerlimit*sd(a.norm$correlation,na.rm=T),],
																					 aes(x = correlation,
																					 		size = 2.5,
																					 		y = name,
																					 		label = name,
																					 		hjust = 0.5+0.5*-sign(correlation)
																					 ),
																					 show_guide = FALSE
																)
	)
	
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(5,1)))
	print(densityPlot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
	print(distributionPlot,vp=viewport(layout.pos.row=2:5,layout.pos.col=1))
	
	return(na.omit(a[a$correlation>upperlimit*sd(a.norm$correlation,na.rm=T) | a$correlation<lowerlimit*sd(a.norm$correlation,na.rm=T),]))
}