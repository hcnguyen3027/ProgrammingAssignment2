## Calculate the inverse of a matrix

## makeCacheMatrix function perform the following methods 
## * set the matrix to cache
## * retrieve matrix from cache
## * set the inverse of the matrix to cache
## * retrieve the inverse matrix from cache



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  
          x <<- y
          m <<- NULL
        }
        get <- function() x   
        setmatrix <- function(solve) m <<- solve   
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
  

}


## The cacheSolve function return the inverse of the matrix. If the inverse matrix is already exist in cache then the function return the cache version
## else it will compute the inverse, store it in to cache, and return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m  
}


##Example runs and result

##  > m <- matrix(c(-1, -2, 1, 1), 2,2)
##  > x <- makeCacheMatrix(m)
##  > x$get()
##  [,1] [,2]
##  [1,]   -1    1
##  [2,]   -2    1
##  > inv <- cacheSolve(x)
##  > inv
##  [,1] [,2]
##  [1,]    1   -1
##  [2,]    2   -1
##  > inv <- cacheSolve(x)
##  getting cached data
##  > inv
##  [,1] [,2]
##  [1,]    1   -1
##  [2,]    2   -1
