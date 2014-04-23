## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(z){
        x <<- z
        inv <<- NULL
    }
    get <- function(){
        x
    }
    setinv <- function (y){
        inv <<- y
    }
    getinv <- function(x){
        inv
    }
    list(get=get, set=set, getinv = getinv, setinv= setinv)
}


## This function return the inverse of a matrix if the matrix is new. 
# If the inverse has already been found, it'll just return the value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}