## makeCacheMatrix creates a "matrix" that cache its inverse. It has a list of 
## functions to set the matrix, get the matrix, set the inverse of matrix and get
## the inverse of matrix. cacheSolve checks if the inverse is calculated for the 
## matrix. If calculated, it returns the inverse else calculates and return the 
## inverse of given matrix.

## makeCacheMatrix is the function that creates the matrix 

makeCacheMatrix <- function(x = matrix()) {
		m<-NULL
		set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get,
	   setmatrix=setmatrix,
	   getmatrix=getmatrix)
}


## cacheSolve calculates the inverse of matrix returned by makeCacheMatrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getmatrix()
	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
	}
	matrix<-x$get()
	m<-solve(matrix, ...)
	x$setmatrix(m)
      m
}
