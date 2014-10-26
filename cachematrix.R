###############################################################################
# makeCacheMatrix() function has a default argument of an empty matrix object
# it creates a cache object
# it has 4 methods:
# 	set             - stores the matrix object it received
# 	get             - returns the stored matrix object
# 	setInvMatrix    - stores inverted matrix 
#   getInvMatrix    - returns the stored inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) stop("This was not a matrix!!!") # error if no matrix 
    m <- NULL      # creates empty m variable within this function
    set <- function(y) {
        # TODO: check there as well if matrix passed here
        x <<- y    # set parent frame (calling environment) var x to y
        m <<- NULL # set parent frame variable m to nothing
        # parent frame here is that of the cacheObject
        # as this function is defined within that function
    }
    get <- function() x             # returns current x value of cacheObject
    setInvMatrix <- function(invM) m <<- invM # stores inverted matrix in m
    getInvMatrix <- function() m    # returns current m value of cacheObject
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
    # publishes cacheObject internal functions (methods)
}

# cacheSolve() function sees if inverse matrix has been cached, and if not
# it computes the inverse matrix and stores it in the cache
cacheSolve <- function(x, ...) {
    m <- x$getInvMatrix()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m) # returns cached matrix object
    }             # execution stopped
    data <- x$get()         # if m was NULL then get the matrix to invert
    m <- solve(data, ...)   # compute the inverted matrix
    x <- x$setInvMatrix(m)  # store the inverted matrix in cacheObject
    m                       # also return the inverse matrix
}

###############################################################################
# More detailed explanation of interaction of the 2 functions:
# a <- makeCacheMatrix(matrix) creates a cacheObject a
#   and initializes it with a matrix to store in x variable
#   and with NULL to store in m variable which will store the inverted matrix
#   also stops exection, if argument was not a matrix (a is not created)
# cacheSolve(a) asks a for its stored inverted matrix m
#   if m is NULL then cacheSolve gets the current x value of cacheObject
#   and computes the inverted matrix, 
#   then asks cacheObject a to kindly store it in m
#   next time the inverted matrix is needed, cacheSolve will get stored m 
#   from the cacheObject
# r0bis########################################################################