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
testpop1 <- list(list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 9, proptwo = 8), front = 0, crowding = 0.2),
								 list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 8, proptwo = 9), front = 1, crowding = 0.2),
								 list(genotype = c(1,1,1,1,1,1), phenotype = c(propone = 7, proptwo = 7), front = 1, crowding = 0.2)
)

testpop2 <- list(list(phenotype = c(propone = 6, proptwo = 4)),
								 list(phenotype = c(propone = 4, proptwo = 4)),
								 list(phenotype = c(propone = 3.9, proptwo = 6)),
								 list(phenotype = c(propone = 3.1, proptwo = 5)),
								 list(phenotype = c(propone = 2.5, proptwo = 3.5)),
								 list(phenotype = c(propone = 3.5, proptwo = 2.3))
)

test_that('reproduce gives a plausible result', {
	pop <- 10
	res <- reproduce(testpop1, pop)
	
	expect_equal(length(res), pop)
})

test_that('mature gives correct result', {
	res <- mature(testpop1, evaluate)
	
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
	expect_equal(res[[1]][['phenotype']], c(propone = 3, proptwo = 3))
})

test_that('nondomsort gives correct result on testpop1', {
	res <- nondomsort(testpop1)
	expect_that(res[[1]][['front']], equals(0))
	expect_that(res[[2]][['front']], equals(0))
	expect_that(res[[3]][['front']], equals(1))
	
	expect_that(res[[1]][['crowding']], equals(mean(c(sqrt(2),sqrt(5)))))
	expect_that(res[[2]][['crowding']], equals(mean(c(sqrt(2),sqrt(5)))))
	expect_that(res[[3]][['crowding']], equals(sqrt(5)))
})

test_that('nondomsort gives correct result on testpop2', {
	res <- nondomsort(testpop2)
	expect_that(res[[1]][['front']], equals(0))
	expect_that(res[[2]][['front']], equals(1))
	expect_that(res[[3]][['front']], equals(0))
	expect_that(res[[4]][['front']], equals(1))
	expect_that(res[[5]][['front']], equals(2))
	expect_that(res[[5]][['front']], equals(2))
})

test_that('select gives correct results', {
	res <- select(testpop1, 2)
	
	expect_that(length(res), equals(2))
	
	expect_that(res[[1]][['front']], equals(0))
	expect_that(res[[2]][['front']], equals(1))
})

test_that('GDMO gives plausible results', {
	res <- GDMO(10, 10, list(list(genotype = c(1,1,1,1,1,1))), evaluate)
})
