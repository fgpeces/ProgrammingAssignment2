## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special"matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {     
        s <- null
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

##Calculate the inverse of the special "matrix" returned by above function. If the inverse has already been calculated and 
##matriz has not changed retrieve the inverse from the cache, otherwise calculate the inverse of the data and sets 
##the value of the inverse via setsolve function
##To check if the matriz has not changed we check that the AxA*=I that should be the same of (AXA*)*=I*=I;
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        s <- x$getsolve()
        if(!is.null(s) && identical(data %*% s,solve(data %*% s))){
                message("getting cached data")
                return(s)
        }
        
        s <- solve(data,...)
        x$setsolve(s)
        s
}
