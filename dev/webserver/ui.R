require(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Pareto Front Explorer"),
  
  sidebarPanel(
#     selectInput(inputId = 'dataset', 
#                 label = 'Dataset:', 
#                 choices = list(
#                   'G. Sulfurreducens' = '1216065400',
#                   'G. Metallireducens' = '1348023600'
#                 )
#     )
  ),
  
  mainPanel(
    h3('Pareto Front'),
    selectInput(inputId = 'xAxis',
                label = 'x-Axis:',
    						choices = list(
    							'Biomass_Ecoli_core_w_GAM' = 'phenotype.Biomass_Ecoli_core_w_GAM',
    							'EX_ac.e.' = 'phenotype.EX_ac.e.',
    							'kos' = 'phenotype.kos'
    						)
    ),
    selectInput(inputId = 'yAxis',
                label = 'y-Axis:',
                choices = list(
                  'Biomass_Ecoli_core_w_GAM' = 'phenotype.Biomass_Ecoli_core_w_GAM',
                  'EX_ac.e.' = 'phenotype.EX_ac.e.',
                  'kos' = 'phenotype.kos'
                )
    ),
    plotOutput('pfrontPlot'),
    
    h3('Heatmap'),
    plotOutput('heatmap'),
    
    h3("Outliers"),
    sliderInput("sdLimits", "Standard Deviation Limits:",
                min = -5, max = 5, value = c(-2,2), step = 0.1
    ),
    plotOutput('outliersPlot')
  )
))