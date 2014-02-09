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
testpop <- list(list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 9, proptwo = 8), dom = 4, crowding = 0.2),
								list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 8, proptwo = 9), dom = 2, crowding = 0.2),
								list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 7, proptwo = 7), dom = 3, crowding = 0.2)
)

test_that('reproduce gives a plausible result', {
	pop <- 10
	res <- reproduce(testpop, pop)
	
	expect_equal(length(res), pop)
})

test_that('mature gives correct result', {
	res <- mature(testpop, evaluate)
	
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
})

test_that('nondomsort gives correct result', {
	res <- nondomsort(testpop)
	expect_that(res[[1]][['dom']], equals(0))
	expect_that(res[[2]][['dom']], equals(0))
	expect_that(res[[3]][['dom']], equals(2))
	
	expect_that(res[[1]][['crowding']], equals(sqrt(2)))
	expect_that(res[[2]][['crowding']], equals(sqrt(2)))
	expect_that(res[[3]][['crowding']], equals(sqrt(5)))
})

test_that('select gives correct results', {
	res <- select(testpop, 2)
	
	expect_that(length(res), equals(2))
	
	expect_that(res[[1]][['dom']], equals(2))
	expect_that(res[[2]][['dom']], equals(3))
})

test_that('GDMO gives plausible results', {
	res <- GDMO(10, 10, list(list(genotype = c(1,1,1,1,1,1))), evaluate)
})
