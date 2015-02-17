## This function will return the inverse of a matrix, but before computing
## the inverse, if check in the cache if this inverse matrix was calculated
## before to avoid re-calculating

## The base function is the same that in the instructions of the assigment,
## I will modify step by step the function to addapt to my desires

makeCacheMatrix <- function(x = matrix()) { ## The input is a matrix
    m <- NULL
    set <- function(y) { ## In the first time that I call the function, it
        x <<- y          ## saves the variable
        m <<- NULL
    }
    get <- function() x ## This is the function to get the value of the variable
    setsolve <- function(solve) m <<- solve ## This is to set the inverse
    getsolve <- function() m ## This is to get the inverse calculated before
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) ## The function returns a list with the four functions mentioned
}


## The base function is the same that in the instructions of the assigment
## I will modify step by step the function to addapt to my desires

cacheSolve <- function(x, ...) { ## The input is the variable asigned with the last function
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve() ## The funcion recover the value from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m) ## If the inverse was calculated, it returns it with a message
    }
    data <- x$get() ## If the inverse is not calculated yet, it recover the data
    m <- solve(data, ...) ## and calculated the corresponding inverse
    x$setsolve(m) ## Then, the value is stored to avoid recurrent calculation
    m
}

## The ussage of this function could be explained with one example.
## If the variable you want to calculate the inverse is:

##   [1 2 3 4 5]
##   [2 3 4 5 1]
## A=[3 4 5 1 2]
##   [4 5 1 2 3]
##   [5 1 2 3 4]

## The code should be:
## 'A <- matrix(c(1,2,3,4,5,2,3,4,5,1,3,4,5,1,2,4,5,1,2,3,5,1,2,3,4),5,5)'
## 'a <- makeCacheMatrix(A)'
## 'cacheSolve(a)'

## The first time, the inverse was calculated, but the next times, the result was cached