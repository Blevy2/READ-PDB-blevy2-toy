#' @title define spawning areas 
#' 
#' @description \code{define_spawn} is an auxiliary function called by
#' \code{create_spawn_hab} to create the spawning habitat preferences.

#' @param coord is a List of Numeric vectors of the boundaries of the spawning
#' areas, i.e. list(spwn1 = c(x1, x2, y1, y2), spwn2 = ...)
#' @param spwn is a Numeric matrix of 1s fed in by \code{create_spawn_hab}
#' @param mult is a Numeric of the attractiveness of the spawning areas

#' @return a matrix of spawning preference 

#' @examples
#' define_spawn(coord = list(spwn1 = c(2,4,2,4)), spwn = matrix(nc = 3, runif(9)), mult = 10)  

#' @export

define_spawn_Bens <- function(coord = NULL, spwn = NULL, mult = 10){

	for(i in seq(length(coord[,1]))) {
	x <- coord[i,1] 
	y <- coord[i,2]
	spwn[x,y] <- spwn[x,y] * mult
	}

	return(spwn)

}

