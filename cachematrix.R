## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. get the matrix
# 2. set the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix
#

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<-NULL
        } 
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set=set,get=get,
             setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
# 1. load original_matrix, and compare it with 
#    matrix data in the current environment
# 2. computer the inverse matrix obtained from 
#    above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                ori <- x$get()
        inv <- x$getinv()
        if ((!is.null(inv)) && identical(x,ori)) {
                message("getting cached data")
                return(inv)
        }else{
                matrix <- x$get()
                inv <- solve(matrix,...)
                x$setinv(inv)
        }
        return(inv)
}
