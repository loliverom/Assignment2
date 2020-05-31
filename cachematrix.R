## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        #Setting the matrix 
        set <- function(y) {
                x <<- y
             inv <<- NULL   
              }
        #function to retrive the matrix
        get <- function () {x}
        
        #set and get the inverse of the matrix
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function(){inv}
        
        #set list and print
        
        list(
        set = set , get=get , setInverse= setInverse,
                getInverse =getInverse
        )
        
}
## First we have to set how the function is going to recognize the original matrix
##later we capture all methods in a list and print it 



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
        message("getting cached data)
        return(inv)
        }
        matrixInfo<- x$get()
        
        inv <- solve(matrixInfo)%*% matrixInfo
        
        x$setInverse(inv)
        
        print(inv)
        
}
##First we evaluate whether the inverse of the matrix has been already calculated 
## the function just return the same matrix; or instead we need to calculate it.
##So we capture the information of the original matrix and later we calculate the inverse using just multiplying matrices
#then we present the results
