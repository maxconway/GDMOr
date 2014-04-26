#' Produce a heatmap
#' 
#' @param dataset a dataframe with some columns prefixed by \code{genotype.},
#'   and some by \code{phenotype.}
#' @param colour the colour for the resulting heatmap
#' 
#' @export
heatmapify <- function(dataset, colour='purple'){
  
  v=as.matrix(1*dataset[order(princomp(dataset[,grep(pattern='phenotype.*',setdiff(colnames(resdf),'phenotype.kos'))])$scores[,'Comp.1']),
                              grep(pattern='genotype.*',
                                   colnames(dataset)
                              )
                              ]
  )
  
  # jitter v slightly to avoid a stack overflow from too manhy identical values
  vmod <- v+matrix(rnorm(prod(dim(v)),sd = 0.0001), nrow = nrow(v), ncol = ncol(v))
  
  # cluster rows
  hc.rows <- hclust(dist(vmod))
  
  # transpose the matrix and cluster columns
  hc.cols <- hclust(dist(t(vmod)))
  
  r <- vmod[,]
  
  heatmap(x=r,
          Rowv=NA,
          Colv=NULL,
          labRow=NA,
          labCol=NA,
          ylab='strain, sorted by distance from WT',
          xlab='gene, clustered by Manhattan distance',
          col=c('ghostwhite',colour),
          scale='none',
          distfun=function(x){dist(x,'manhattan')}
  )
  
}