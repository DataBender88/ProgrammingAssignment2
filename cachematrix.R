## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverseMatrix <<- solve
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}

# > ## Test run:
# > ## Create matrix
# > x = rbind(c(1,-2),c(-2,1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1   -2
# [2,]   -2    1
# > ## Create inverse matrix
# > cacheSolve(m)
# [,1]       [,2]
# [1,] -0.3333333 -0.6666667
# [2,] -0.6666667 -0.3333333

