context('MMO code')

## create a sample optimization problem

# evaluation function:
evaluate <- function(x){
	c(propertyone = abs(x-c(1,1,1,0,0,0)), 
		propertyone = abs(x-c(0,0,0,1,1,1))
	)
}
# starting point:
x <- list(genotype = c(0,0,0,0,0,0))

## run a test using it