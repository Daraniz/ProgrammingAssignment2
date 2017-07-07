## Put comments here that give an overall description of what your
## functions do

## This function creates an object with two elements: a matrix and its inverse. 
## It has 4 functions: 1 Write the matrix element. 2 Get the matrix element.
## 3 Write the inverse matrix element. 4 Get the inverse matrix element.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) invmat <<- inversematrix
        getinversematrix <- function() invmat
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
        
}


## This function takes an object created by the previous function and caches the
## inverse matrix. If the object doesn't have the inverse calculated, the function
## calculates it. If the object has the inverse calculated, the function returns
## the inverse stored in the object.

cacheSolve <- function(x, ...) {
                invmat <- x$getinversematrix()
                if(!is.null(invmat)) {
                        message("getting cached data")
                        return(invmat)
                }
                data <- x$get()
                invmat <- solve(data,...)
                x$setinversematrix(invmat)
                invmat
        }

