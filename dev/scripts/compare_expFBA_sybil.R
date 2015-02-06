library(metabex)
library(sybil)
library(plyr)
library(dplyr)
library(ggplot2)

data('geo_m', 'geo_m', 'geo_m_model', 'geo_s_model', package='metabex')

compare_expFBA_sybil <- function (model, expFBA_output, objectives) {
  model@obj_coef <- 1000000*(model@react_id==objectives[1])
  sybil_fluxes <- geneDeletion(model, fld='fluxes', combinations = 1)@fluxdist@fluxes
  sybilresults <- sybil_fluxes %>% t() %>% as.matrix() %>% as.data.frame()
  colnames(sybilresults) <- model@react_id
  
  expFBA_expressions <- expFBA_output %>% select(starts_with('genotype.'))
  
  expFBAresults <- plyr::ddply(expFBA_expressions, ~ 1:nrow(expFBA_expressions),  .progress = "text", function(individual){
  	as.data.frame(as.list(mature(model, genes2bounds(model, as.numeric(as.list(individual))), objectives)), optional=TRUE)
  })
  
  results <-rbind_list(
  	expFBAresults %>% 
  		select(one_of(objectives)) %>% 
  		mutate(technique='expFBA'),
  	sybilresults %>% 
  		select(one_of(objectives)) %>% 
  		mutate(technique='sybil')
  	)
  
  return(results)
}

results <- compare_expFBA_sybil(geo_s_model, geo_s, objectives=c("agg_GS13m", "EX_fe2(e)"))
ggplot(results, aes(x=agg_GS13m, y=`EX_fe2(e)`, colour=technique, fill=technique)) + 
	geom_point() + 
	annotate(geom = 'text', label='wild-type', x=0.1016639-0.01, y=1.760785+0.1) +
	annotate(geom='line', x=c(0.1016639, 0.1016639-0.005), y=c(1.760785, 1.760785+0.1))

results <- compare_expFBA_sybil(geo_m_model, geo_m, objectives=c("biomass", "EX_fe2(e)"))
ggplot(results, aes(x=biomass, y=`EX_fe2(e)`, colour=technique, fill=technique)) + geom_point() + xlim(0,0.1)
