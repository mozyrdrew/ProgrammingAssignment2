## Put comments here that give an overall description of what your
## functions do

## this function creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ##define inverse
        inv <- NULL
        
        ##method set matrix
        set <- function(matrix){
                mx <<- matrix
                i <<- NULL
        }
        
        ##method get matrix
        get <- function(matrix){
                mx
        }
        
        ##method set inverse
        setInverse <- function(inverse){
                inv <<- inverse
        }
        
        ##method get matrix
        getInverse <- function(){
                inv
        }
        
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
        

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mx <- x$getInverse()
        
        ## Check whether inverse is already set
        if(!is.null(mx)){
                message("getting cached data")
                return(mx)
        }
        
        ## Get matrix
        data <- x$get()
        
        ## Compute the inverse 
        mx <- solve(data) %% data
        
        ## Set the inverse
        x$setInverse(mx)
        
        ## Return matrix
        mx
        
}
