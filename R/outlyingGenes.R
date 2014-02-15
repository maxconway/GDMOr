require(ggplot2)
require(grid)
outlyingGenes <- function(dataset, lowerlimit=-2, upperlimit=2){
  
	dataset$pos <- princomp(dataset[,grep(pattern='phenotype.*',setdiff(colnames(resdf),'phenotype.kos'))])$scores[,'Comp.1']
	
  a <- do.call(rbind,lapply(grep(pattern='genotype.*',colnames(dataset),value=T),function(column){
    data.frame(name=column,correlation=cor(dataset[,column],dataset$pos))
  }))
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
                                +geom_text(data=a[a$correlation>upperlimit*sd(a.norm$correlation,na.rm=T) | a$correlation<lowerlimit*sd(a.norm$correlation,na.rm=T),], aes(x=correlation,size=2.5,y=name,label=name,hjust=0.5+0.5*-sign(correlation)), show_guide=FALSE)
  )
  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(5,1)))
  print(densityPlot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
  print(distributionPlot,vp=viewport(layout.pos.row=2:5,layout.pos.col=1))
  
  return(na.omit(a[a$correlation>upperlimit*sd(a.norm$correlation,na.rm=T) | a$correlation<lowerlimit*sd(a.norm$correlation,na.rm=T),]))
}