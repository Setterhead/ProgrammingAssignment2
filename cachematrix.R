## Tab width = 4


## I created two functions: 'makeCache' and 'cacheSolve'. The first
## one requires a matrix to be inputed and then creates
## a special vector (list) of functions, which are used by the second
## function. And the second uses output of the first in order to return
## an inverse matrix for the inputed one. Note that if calculation
## for a matrix has been already completed, 'cacheSolve' won't run it
## again.Instead it will use previously cached data.

## 'makeCacheMatrix' creates a list of functions (described in the body
## of the function below) which will be later operated by the 'cacheSolve'
## function

makeCacheMatrix <- function(x = matrix()) {
    ## initially nothing is inversed so set 'inverse' to NULL
    inverse <- NULL
    ##creating a function that allows to set the new value of inputed 'x'
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## creating a function that returns the inputed vector 'x'
    get <- function() x
    ## creating a function that allows to set the value of 'inverse'
    setinv <- function(inv) inverse <<- inv
    ## creating a function that prints 'inverse'
    getinv <- function() inverse
    ## now you get a list of all previously created functions which 
    ## will be used in the next function (cacheSolve)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## 'cacheSolve' function checks if inverse matrix have been already
## calculated for the inputed matrix. And if so prints previously cached
## result.Else calculates the inverse matrix, saves it to cache and
## prints the result aswell

cacheSolve <- function(x, ...) {
    ## checks if a matrix that is the inverse of 'x' has been already
    ## calculated and cached
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        ## if it has been chached indeed, prints message and returns 
        ## special result from the cache
        message("getting cached data")
        return(inverse)
    }
    ## else uses the list created by 'makeCacheMatrix' in order to
    ## get the input (1st line), calculate inversed matrix (2nd line)
    ## cache inversed matrix (3rd line) and return it (4th line)
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}

## I beg pardon for my english^^
