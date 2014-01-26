GDMO <- function(pop, gen, startingpoint, evaluate){
	parents <- lapply(1:pop,function(x){startingpoint})
	currentgen = 0
	while(currentgen <= gen){
		children <- reproduce(parents = parents)
		population <- nondomsort(list(mature(children, evaluate), parents))
		parents <- select(population, maxpop = pop)
		currentgen <- currentgen + 1
	}
}