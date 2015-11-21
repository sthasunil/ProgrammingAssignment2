## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## maintains the cache of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## gets the inverse of matrix from caching
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
          message("getting cached data")
          return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}

##Test
"
B <- matrix( c(2, 4, 3, 1, 5, 7, 1, 2, 3),  nrow=3, ncol=3)
B_inverse <- solve(B)
message('Actual Inverse from R')
B_inverse

cm <- makeCacheMatrix(B)
message('Implemented cached Inverse from R')
inverse1 <- cacheSolve(cm)
inverse2 <- cacheSolve(cm)
"
