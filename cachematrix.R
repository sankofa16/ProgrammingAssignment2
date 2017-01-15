## Put comments here that give an overall description of what your
## functions do

# ---- The 1st function makes a matrix objection that can
# store/cache its inverse. It contains 4 functions in it ()
# ---- The 2nd function calculates the inverse of the matrix made by
# the 1st function. If matrix is the same + the inverse was already calc'd
# then the 2nd function just returns the stored/"cached" inverse.


## Write a short comment describing this function
# ---- The function makes a matrix that can cache (store) the inverse 
# and contains 4 functions: set, get, setInverse, and getInverse

makeCacheMatrix <- function(x = matrix()) {
    sushi <- NULL
    # 1) SET
    set <- function(y) {
        x <<-  y #similar to makeVector example
        sushi <<- NULL
    }
    # GET. Similar to MakeVector ex but "inverse" instead of "mean"
    get <- function() x
    setInverse <- function(inverse) sushi <<-- inverse
    getInverse <- function() sushi
    list(set= set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# --- it calculates the inverse of the matrix from makeCacheMatrix
# above. if already calc'd then it inverse is retrieved from the cache
# and the message "getting cached data" will appear as well

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sushi <- x$getInverse()
    if(!is.null(sushi)) {
        message("getting cached data")
        return(sushi)
    }
    # using solve() which finds the actual inverse
    # e.g., solve(blahblah_matrix) will give you the inverse of blahblah_matrix
    data <- x$get()
    sushi <- solve(data, ...)
    x$setInverse(sushi)
    sushi
}

# --- Below is some sample code for testing this out if you un-comment
#test_matrix <- diag(7,4)  ##-- makes a matrix that is 4x4 with "7" diagonal
# test_matrix ## look at the matrix you made!
# cached_matrix <- makeCacheMatrix(test_matrix) ## the 1st function
		## now put it in the 2nd function
#cacheSolve(cached_matrix)
		## you'll see the inverse matrix, also 4x4 
		## now if we try to run cacheSolve(cached_matrix) AGAIN
		## w/o changing the cached_matrix, we will get a messsage saying
		## "getting cached data" - letting us know this inverse wasn't
		## calc'd again from scratch, but already calc'd and cached.
# cacheSolve(cached_matrix)


