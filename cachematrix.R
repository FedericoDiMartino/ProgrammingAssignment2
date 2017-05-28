## makecacheMatrix sets up the cache and the functions required to access it ..
## ... from the parent environment

makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL      ## Initialises cache and clears it 
    
    set <- function(y) {        ## Setter function for input x
        x <<- y                 ## Sets x in parent environment
        cachedinverse <<- NULL ## Clears cache in parent environment
    }
    
    get <- function() x         ## Retrieves x from parent environment
    
    setinverse <- function(inverse) cachedinverse <<- inverse  ## Sets inverse in parent environment
    
    getinverse <- function() cachedinverse     ## Retrieves cache from parent environment
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
    ## Generates list of functions with each element named after the respective function      
    
    
    
}


## cacheSolve checks if the inverse of the input matrix has been calculated ..
## .. if yes, it returns that value, otherwise it calculates it from scratch

cacheSolve <- function(x, ...) {    ## Returns a matrix that is the inverse of 'x'
    cachedinverse <- x$getinverse()  ## Retrieves cache
    
    if(!is.null(cachedinverse)){   ## Checks if cached value is null
        return(cachedinverse)      ## If cache is NOT null, returns cache as result and stops function here
    }
    else {                              ## If cache is null will newly calculate inverse
        input <- x$get()                  ## Retrieves input matrix to invert
        cachedinverse <- solve(input)  ## Inverts input and saves it to cache  
        x$setinverse(cachedinverse)    ## Puts new cache into input
        return(cachedinverse)          ## Outputs result
    }
    
    
}
