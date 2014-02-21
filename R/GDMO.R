#' Performs GDMO
#' 
#' @param population int, the population to be used.
#' @param generations: int, the number of generations to run for
#' @param startingpoint: a list with a binary element genotype
#' @param evaluate: and evaluation function
#'   
#' @return a list of pareto optimal solutions, each member being a list with
#'   elements phenotype and genotype
#'   
#' @export
#' @import lubridate
GDMO <- function(population, generations, startingpoint, evaluate, savefile = 'checkpoint.RData'){
	parents <- startingpoint
	currentgen <- 1
	
	genbar <- txtProgressBar(min = 0, max = generations, style=2)
	starttime <- now()
#	tryCatch(
		while(currentgen <= generations){
			children <- reproduce(parents = parents, pop = population)
			adults <- nondomsort(c(mature(children, evaluate), parents))
			parents <- select(adults, maxpop = population)
			currentgen <- currentgen + 1
			if((now() - starttime) > new_difftime(seconds = 30) && currentgen%%10==0){
				save(parents, file = savefile)
			}
			setTxtProgressBar(genbar, currentgen)
			print(now() + (now()-starttime)*(generations/currentgen-1))
		}#,
#		finally = return(parents)
#	)
}