##	Input variables:
##	1. x: input Square Invertible Matrix
## 	Output variables:
##	return a list of 4 functions to:
##	1. Set the value of the matrix
##	2. Get the value of the matrix
##	3. Set the value of the inverse
##	4. Get the value of the inverse 

##	Operation example:
##	> x <- cbind(c(2,0), c(0, 2))		// Create a test matrix x
##	> ax <- makeCacheMatrix(x)          // Create the special matrix
##	> ax$get()                          // Get the input matrix
##	> cacheSolve(ax)                    // Compute the inverse
##	> cacheSolve(ax)                    // Call again with same x, so return
##    						// the cached inverse
##	> ax$set(cbind(c(3,0), c(0, 3)))	// set a new input for computation
##	> cacheSolve(ax)                    // Compute the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# xinv will store the cached inverse matrix
	# So for first execution of makeCacheMatrix, there is no xinv available in cache
	xinv <- NULL

	# Set a new or any value of for the matrix, x.
	# In this case, we can not use cached inverse, hence xinv is NULL 
	set <- function(y) {
      	x <<- y
		xinv <<- NULL
	}

	# Get the matrix
	get <- function() x

	# Set the inverse 
	setinv <- function(inverse) xinv <<- inverse

	# Get the inverse
	getinv <- function() xinv

	# Return the 4 function lists that operate on matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

# cacheSolve: Inverse calculation of the matrix. Assuming that the matrix
# is invertible. Returns cached inverse if a new/same matrix is set with set
# function
cacheSolve <- function(x, ...) {
	xinv <- x$getinv()

	# If the inverse is already calculated, return it
	# This will happen second execution onwards
	if (!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
    	}

	# The inverse is not yet calculated, so we calculate it
	# This will happen for the first executioin or 
	# whenever there is a change in matrix, x
	data <- x$get()
	xinv <- solve(data, ...)

	# Put the inverse, xinv in Cache
	x$setinv(xinv)

	# Return inverse xinv
	xinv
}
