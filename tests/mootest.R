context('MMO code')

## create a sample optimization problem

# evaluation function:
evaluate <- function(x){
	c(propone = sum(abs(x-c(1,1,1,0,0,0))), 
		proptwo = sum(abs(x-c(0,0,0,1,1,1)))
	)
}
# starting point:
x <- list(genotype = c(0,0,0,0,0,0))
# sample population
testpop <- list(list(genotype = c(1,1,1,1,1,1,1), phenotype = c(propone = 9, proptwo = 9)),
								list(genotype = c(1,1,1,1,1,1,1), phenotype = c(propone = 9, proptwo = 9)),
								list(genotype = c(1,1,1,1,1,1,1), phenotype = c(propone = 9, proptwo = 9))
								)

## run a test using it