## This functions accepts matrix as an argument with an assumption,
## it's reversible and returns the list of setters and getters functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y    
                ## to comply with '(and the matrix has not changed)' requirement
                i <<- NULL             
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)

}


## This functions accepts the matrix created by makeCacheMatrix 
## function as an argument, checks if inverse is already
## calculated for given matrix x (and if it's not changed) and if
## so, returns the cached result. Else, it will calculate the inverse
## of a matrix and cache the result for future re-use.

cacheSolve <- function(x, ...) {       
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
