## The first function, makeMatrix creates a "matrix"
## Really it is a list containing a function to:
##      1. set the value of the matrix
##      2. get teh value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve cacluates
## the inverse of the "matrix" created with
## the makeMatrix function defined above. 
## But it first checks if the inverse has
## already been calculated. If it has, this function
## retrieves the inverse from the cache and 
## skips doing a new computation. If it hasn't, this
## function cacluates the inverse of the matrix and
## sets the value of the inverse in the cache via
## setinverse().

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#test
#matrix <- matrix(c(1:3, 2:4), nrow = 2, ncol = 2)
#matrix
#m1 <- makeMatrix(matrix)
#m2 <- cacheInverse(m1)
#matrix %*% m2
