## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## The first function, 'makeCacheMatrix' creates a special "matrix", which
## is really a list containing a function to 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inversion
## 4.  get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
	v <-NULL                                      ## initialize v                 
	set <- function(y){                           ## assign values in parent environment
		x <<- y                                                   
		v <<- NULL
	}
	get <- function() x                           ## get value of matrix x
	setivs <- function(ivs) v <<- ivs             ## set the inversion
	getivs <- function() v                        ## get the inversion
	list(set = set, get = get,                   
	     setivs = setivs, getivs = getivs
	     ) 
}


## The following function solves the inversion of the special "matrix"
## created with the above function. However, it first checks to see if the
## inversion has already been solved. If so, it `get`s the inversion from the
## cache and skips the computation. Otherwise, it solves the inversion of
## the data and sets the value of the inversion in the cache via the `setivs`
## function.


cacheSolve <- function(x, ...) {
	v <- x$getivs()                                 
	if(!is.null(v)){                                ## if the inverse is solved before,
		message("getting cached data")          ## then get result from cached data
		return(v)
	}
	data <- x$get()                                 ## if not, solve the inversion
	v <- solve(data, ...)
	x$setivs(v)
	v
}
