## Put comments here that give an overall description of what your
## functions do

## Create a special matrix, which is a list containing 4 functions
## 1) Set the value of a matrix
## 2) Get the value of a matrix
## 3) Set the inverse of a matrix
## 4) Get the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
    ## Inv variable to hold inverse matrix
    inv <- NULL
    
    ## Set function, using << operators to keep the value once set
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## Get function, returns matrix
    get <- function() x
    
    ## Set inverse, uses << operator to kep the value once set
    setinverse <- function(inverse) inv <<- inverse
    
    ## Get inverse , retrives inverse matrix
    getinverse <- function() inv
    
    ## return list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Returns the inverse of the special matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## If the inverse matrix has already been calculated
    ## Retrieve inverse matrix instead of re-calculating 
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieved from cached data")
        return(inv)
    }
    
    ##If inverse matrix has not been else
    ## Get the matrix data, calacuate inverse
    ## Set in special matrix, and return inverse matrix
   
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
       
}
