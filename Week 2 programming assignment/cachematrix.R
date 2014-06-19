## complementary functions to create a cache for storing matrix inverse
## and having another function to retrieve results from cache if exists. 
## if not, then to calculate value. Uses lexical operator to update function.

## creates a special "matrix" object that can cache its inverse and hold value.
## 1. sets the value of the matrix and defaults the value of inverse
## 2. gets the value of matrix
## 3. sets the value of the inverse
## 4. gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        glob_inv <- NULL
        set <- function(y) {
                x <<- y
                glob_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) glob_inv <<- inv
        getinv <- function() glob_inv
        list(fxn1 = set, fxn2 = get,
             fxn3 = setinv,
             fxn4 = getinv)
}


## function that computes the inverse - either from cache matrix if exists,
## or new computation
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inverse <- x$fxn4()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$fxn2()
        inverse <- solve(data, ...)
        x$fxn3(inverse)
        inverse
}
