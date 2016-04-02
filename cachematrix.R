#The first function, makeVector creates a special "vector", which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #    set the value of the vector
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #    get the value of the vector        
        get <- function() x
        #    set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        #    get the value of the inverse
        getinverse <- function() inv
        #create the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The second function calculates the mean of the special "vector" created with the first function. 
cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        #It first checks to see if the mean has already been calculated. 
        if(!is.null(inv)) {
        #If so, it gets the mean from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        #Otherwise, it calculates the mean of the data 
        data <- x$get()
        inv <- solve(data, ...)
        #and sets the value of the mean in the cache via the setmean function.  
        x$setinverse(inv)
        inv
}
