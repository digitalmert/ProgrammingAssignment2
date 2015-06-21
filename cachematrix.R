## Put comments here that give an overall description of what your
## functions do

## This function takes an invertible matrix as its argument and returns a list of functions to get the matrix, set the matrix, get the inverse of the matrix, and set the inverse of the matrix. 
##get() function returns the matrix we have provided to the function makeCacheMatrix and getInverseMatrix() returns the inverse of the matrix.
##set() function takes a matrix and assigns it to the variable x which is defined in the parent environment of it so that its value is redefined.
##setInverseMatrix() function takes the inverse of a matrix and assigns it to the variable m which is defined in its parent environment.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		  x <<- y
		  m <<- NULL
		}
	get <- function() x
	setInverseMatrix <- function(inverse) m <<- inverse
	getInverseMatrix <- function() m
	list(set=set, get=get,
	   setInverseMatrix=setInverseMatrix,
	   getInverseMatrix=getInverseMatrix)
}

##This function takes as its argument a list of functions returned by makeCacheMatrix function  and returns the inverse of the matrix which has been provided to makeCacheMatrix function. 
##It first calls the getInverseMatrix function of the list x and checks whether it returns null or not. If it is not null, than it means the inserve of the matrix has been already calculated and it gets the inverse from the cache. Otherwise, it computes the inverse of the matrix,  and returns it. 

cacheSolve <- function(x, ...) {
    m <- x$getInverseMatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    the_matrix <- x$get()
    m<-solve(the_matrix, ...)
    x$setInverseMatrix(m)
    m
}