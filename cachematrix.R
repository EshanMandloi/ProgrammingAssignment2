## makeCacheMatrix() creates a special matrix object that
## can cache its inverse and cacheSolve() gives the inverse

## in makeCacheMatrix() x is the input whose inverse is to be evaluated 

makeCacheMatrix <- function(x = matrix()) {

	inx <- NULL
	set <- function(y){
		x <<- y
		inx <<- NULL
	}
	get <- function() x
	setin <- function(inv) inx <<- inv
	getin <- function() inx
	list(set = set, get = get, setin = setin, getin = getin)
}


##cacheSolve() takes the matrix object created form makeCacheMatrix() and 
##returnes the iverse of the matrix

cacheSolve <- function(x, ...) {
	
	inv <- x$getin()
	if(!is.null(inv)){
		message("getting cache data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setin(inv)
	inv	
}
