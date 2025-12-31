# ========== Complete Code ==========

# Define makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    # Initialize inverse matrix cache as NULL
    inv <- NULL
    
    # Define set function - updates matrix and clears cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Define get function - returns the current matrix
    get <- function() x
    
    # Define setinverse function - stores the inverse matrix in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # Define getinverse function - retrieves cached inverse matrix
    getinverse <- function() inv
    
    # Return a list containing all methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Define cacheSolve
cacheSolve <- function(x, ...) {
    # Try to get inverse matrix from cache
    inv <- x$getinverse()
    
    # If cache exists, return it directly
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If cache doesn't exist, get the original matrix
    data <- x$get()
    
    # Calculate the inverse matrix
    inv <- solve(data, ...)
    
    # Store the result in cache
    x$setinverse(inv)
    
    # Return the inverse matrix
    inv
}

# ========== Test Code ==========

# Test 1: Basic functionality
cat("Test 1: Basic functionality\n")
m <- matrix(c(4, 3, 3, 2), 2, 2)
cm <- makeCacheMatrix(m)

cat("First calculation:\n")
inv1 <- cacheSolve(cm)
print(inv1)

cat("\nSecond call:\n")
inv2 <- cacheSolve(cm)
print(inv2)

# Test 2: Verify correctness
cat("\nTest 2: Verify results\n")
result <- m %*% inv1
print(result)  # Should be identity matrix

# Test 3: Modify matrix
cat("\nTest 3: Modify matrix\n")
cm$set(matrix(c(1, 0, 0, 1), 2, 2))
inv3 <- cacheSolve(cm)
print(inv3)
