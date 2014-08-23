## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        m <<- y
        inv <<- NULL
    }
    getMatrix <- function() m
    setInvMatrix <- function(inverse) inv <<- inverse
    getInvMatrix <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(f,x=matrix()) {
    m <- f$getMatrix()
    c <- f$getInvMatrix()
    if(!is.null(c)) {
        if(identical(x,m)) {
            message("getting cached data")
            return(c)
        }      
    }
    i <- solve(x)
    f$setMatrix(x)
    f$setInvMatrix(i)
    i
    ## Return a matrix that is the inverse of 'x'
}

c=rbind(c(1, -1/4), c(-1/4, 1))  
f=makeCacheMatrix()
cacheSolve(f,c)
cacheSolve(f,c)
