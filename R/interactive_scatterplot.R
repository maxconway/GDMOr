#' @title Select a box from a scatterplot
#'   
#' @description Creates a scatter plot, then allows the user to select two
#'   points to define a rectangle of selected points.
#'   
#' @param dataset a data frame
#' @param x,y column names from the data frame to be plotted
#' 
#' @return a logical vector indicating if the points are in the box.
#'   
#' @import ggplot2 ggmap
box_selector <- function(dataset, x, y){
	
	bg <- ggplot(dataset, aes_string(x=x, y=y))
	
	print(bg + geom_point())
	
	print('click twice on the graph to define a rectangle containing the points of interest')
	pts <- gglocator(n=2, message=TRUE)
	
	selected <- (dataset[[x]]<=max(pts[[x]]) &
							 	dataset[[x]]>=min(pts[[x]]) & 
							 	dataset[[y]]<=max(pts[[y]]) &
							 	dataset[[y]]>=min(pts[[y]]))
	
	print(bg + geom_rect(aes(xmax=max(pts[[x]]),
										 xmin=min(pts[[x]]),
										 ymax=max(pts[[y]]),
										 ymin=min(pts[[y]])), alpha=0.9) + geom_point(aes(colour=selected)))
	
	return(selected)
}

#' @title divid a scatter plot by a line
#'   
#' @description Creates a scatter plot, then allows the user to select two
#'   points to define a line.
#'   
#' @param dataset a data frame
#' @param x,y column names from the data frame to be plotted
#' 
#' @return a logical vector indicating if the points are 
#'   
#' @import ggplot2 ggmap
line_selector <- function(dataset, x, y){
	
	bg <- ggplot(dataset, aes_string(x=x, y=y))
	
	print(bg + geom_point())
	
	print('click twice on the graph to define a line dividing the points')
	points <- gglocator(n=2)
	
	vec <- points[2,] - points[1,]
	slope = vec[,y] / vec[,x]
	intercept = points[1,y] - slope*points[1,x]
	
	datavecs <- dataset[,c(x,y)] - points[rep.int(1,nrow(dataset)),]
	side <- vec[,x]*datavecs[,y] > vec[,y]*datavecs[,x]
	
	print(bg + geom_abline(slope=slope, intercept=intercept, linetype=2) + geom_point(aes(colour=side)))
	return(side)
}

