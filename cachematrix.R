## This functions calculate and cache inverse value matrix

## This function creates a special "matrix" object that can cache its inverse.
## This function contains 4 methods: set - set current value matrix,
## get - get current value matrix, setInverse - set value inverse matrix,
## getInverse - get value inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inverseX <- NULL
	set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    
    get <- function() x

    setInverse <- function(value) inverseX <<- value

    getInverse <- function() inverseX

    list(
    	set = set, 
    	get = get, 
    	setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function calculate inverse value input special matrix 
## used R - function solve
## Return a set in input object a matrix that is the inverse of input
cacheSolve <- function(x, ...) {
	inverseX <- x$getInverse()

    if(!is.null(inverseX)) {
        return(inverseX)
    }

    inverseX <- solve(x$get(), ...)
    x$setInverse(inverseX)
    inverseX
}
