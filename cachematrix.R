## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes an invertible matrix as its argument and returns
##a list of functions to get the matrix, set the matrix, get the inverse of 
##the matrix, and set the inverse of the matrix. 
##get() function returns the matrix we have provided to the function 
##makeCacheMatrix and getInverseMatrix() returns the inverse of the matrix.
##set() function takes a matrix and assigns it to the variable x which is
##defined in the parent environment of it so that its value is redefined.
##setInverseMatrix() function takes the inverse of a matrix and assigns it
##to the variable i which is defined in its parent environment.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		  x <<- y
		  i <<- NULL
		}
	get <- function() x
	setInverseMatrix <- function(inverse) i <<- inverse
	getInverseMatrix <- function() i
	list(set=set, get=get,
	   setInverseMatrix=setInverseMatrix,
	   getInverseMatrix=getInverseMatrix)
}


##cacheSolve function takes as its argument a list of functions returned by 
##makeCacheMatrix function  and returns the inverse of the matrix which 
##has been provided to makeCacheMatrix function. 
##It first calls the getInverseMatrix function of the list x and checks
##whether it returns null or not. If it is not null, than it means the 
##inserve of the matrix has been already calculated and it gets the inverse
##from the cache. Otherwise, it computes the inverse of the matrix, and
##returns it. 

cacheSolve <- function(x, ...) {
    i <- x$getInverseMatrix()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    the_matrix <- x$get()
    i<-solve(the_matrix, ...)
    x$setInverseMatrix(i)
    i
}