GDMO <- function(pop,gen,startingpoint){
	parents <- startingpoint
	while(currentgen <= gen){
		children <- reproduce(parents = parents)
		population <- nondomsort(list(mature(children),parents))
		parents <- select(population, maxpop = pop)
	}
}