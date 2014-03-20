require(shiny)
require(ggplot2)
require(grid)
require(plyr)

source('../R/heatmapify.R')
source('../R/outlyingGenes.R')

targetFluxes <- c('Biomass_Ecoli_core_w_GAM', 'EX_ac(e)')
load(file='../temp/checkpoint.RData')

resdf <- ldply(.data = parents, .progress = 'text', .fun = function(individual){
	data.frame(
		genotype = individual$genotype,
		phenotype = {names(individual$phenotype) <- c(targetFluxes, 'kos'); as.list(individual$phenotype)},
		dom = individual$front,
		crowding = individual$crowding
	)
})

shinyServer(function(input, output) {
  
  output$outliersPlot <- renderPlot({
    outlyingGenes(resdf,input$sdLimits[1],input$sdLimits[2])
  })
  
  output$pfrontPlot <- renderPlot({
    plot(ggplot(resdf, aes_string(x=input$xAxis,y=input$yAxis)) +
           geom_point()
    )
  })
  
  output$heatmap <- renderPlot({
    heatmapify(resdf)
  })
  
})