## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {

    ## Initializing Cache Matrix
    
    ## creating variable for storing both initial matrix and its inverse
    cachedM <- NULL
    cachedIM <- NULL

    ## this function returns the cached initial matrix
    getCacheMatrix <- function() cachedM
    
    ## this function stores the initial matrix in the cache
    setCacheMatrix <- function(y) cachedM <<- y

    ## this function returns the cached inverted matrix
    getCacheInvMatrix <- function() cachedIM
    
    ## this function stores the inverted matrix in the cache
    setCacheInvMatrix <- function(y) cachedIM <<- y
    
    ## initializing, the cache for initial matrix (x parameter)
    ## leaving the inverted matrix cache as NULL for the moment
    setCacheMatrix(x)
    
    ## returning a the 4-function vector
    list(setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix, setCacheInvMatrix = setCacheInvMatrix, getCacheInvMatrix = getCacheInvMatrix)
    
}

cacheSolve <- function(x, ...) {
    ## Makes use of "special" cache vector ( x parameter )
    ## Returns a matrix that is the inverse of 'x'
    ## Store the value of the inverse of x, 

    ## testing if x is properly initialized
    if(is.null(x)) {
        message("parameter is not initialized")
        return(NULL)
    }
    
    ## Getting the stored Inverted matrix.
    m <- x$getCacheInvMatrix()
    
    ## Is an Inverted Matrix already stored ?
    if(!is.null(m)) {
        ## Sending a message and return the *stored* inverted matrix
        message("getting Inverted Matrix from cache")
    
        ## Exiting function 
        return(m)
    }
    
    ## at this point, calculating the Inverted is required
    minv <- solve(x$getCacheMatrix())
    
    ## Storing the inverted matrix for future use.
    x$setCacheInvMatrix(minv)
    
    ## Sending a message and return the *calculated* inverted matrix
    message("calculating Inverted Matrix and store it in cache")
    
    ## Exiting function
    return(minv)
}

test_me_please <- function () {
    
    ## Small test function 
    ## initializing an inversible 3x3 matrix
    ## cache it, inverse it twice.
    
    message("witm = matrix(c(1,0,-4,0,1,0,0,0,1),nrow=3,ncol=3)")
    witm = matrix(c(1,0,-4,0,1,0,0,0,1),nrow=3,ncol=3)
    print(witm)
    
    
    message("x<-makeCacheMatrix(witm)")
    x <- makeCacheMatrix(witm)
    
    message("first call to cacheSolve(x)")
    y <- cacheSolve(x)
    print(y)
    
    message("second call to cacheSolve(x)")
    y <- cacheSolve(x)
    print(y)
}
    
