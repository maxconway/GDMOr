# Prediction script

library(plyr)
library(dplyr)
library(ggplot2)
library(metabex)
library(miner)

data('geo_s', 'geo_m', package='metabex')

mod <- mining(phenotype.fe2 ~ .,
data= geo_s %>% select(starts_with('genotype.'), phenotype.fe2),
method=c('holdoutrandom', vpar=0.9),
model='cubist',
task='reg'
)

ggplot(NULL, aes(x=mod$test[[1]], y=mod$pred[[1]])) + geom_jitter()

mod <- fit(phenotype.fe2 ~ .,
data= geo_s %>% select(starts_with('genotype.'), phenotype.fe2),
model='cubist',
task='reg'
)

geo_s %>% select(one_of(c(mod@object$vars$used %>% setdiff(c('committee', 'rule'))), 'phenotype.fe2')) %>% arrange(phenotype.fe2) %>% scale() %>% heatmap(NA)
