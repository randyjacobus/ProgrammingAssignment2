## These functions will return the inverse of a matrix. However, they will first check to see if the inverse has already been calculated and if so it will return it from the cache.


## This function will take an inversible numeric matrix and return a list of of four items.
## The items are are for functions set, get, setinverse, and getinverse which will be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL				# initialize m
        	set <- function(y) {		# sets matrix into cache
                x <<- y			
                m <<- NULL
        }
        get <- function() x		# gets matrix
        setinverse <- function(solve) m <<- solve 	# sets the inverse in cache
        getinverse <- function() m		# gets the inverse from cache
        list(set = set, get = get,		# list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is a client function that expects that the special matirix created from makeCacheMatrix and the output is the inverse. This output is only computed if it is not found in the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the invers of 'x'
        m <- x$getinverse()	# query cache
        if(!is.null(m)) {		# check to see if cache exists
                message("getting cached data") 
                return(m)		# return the cache
        }
        data <- x$get()		# if no cache
        m <- solve(data, ...)	# compute inverse
        x$setinverse(m)		# save in cache
        m
}
