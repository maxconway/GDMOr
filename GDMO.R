GDMO <- function(pop, gen, startingpoint, evaluate){
	parents <- startingpoint
	while(currentgen <= gen){
		children <- reproduce(parents = parents)
		population <- nondomsort(list(mature(children, evaluate), parents))
		parents <- select(population, maxpop = pop)
	}
}