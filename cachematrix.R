# Templated after makeVector & cachevector from Roger Peng's R Programming course.
# https://class.coursera.org/rprog-003/human_grading/view/courses/972138/assessments/3/submissions

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the solve function
## get the value of the solve function

## Set up a specia list of functions that allow caching results of a solve function.
makeCacheMatrix <- function(x = numeric()) {
    # Ensure s isn't set locally.
    s <- NULL
    
    # Create the "set" function which pushes y into the parent environment and clears s.
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Create the" get function
    get <- function() x
    
    # Create the "setsolve" function which sets s to the "solve" function in the parent env.
    setsolve <- function(solve) s <<- solve
    
    # Create the "getsolve" function which retreives s
    getsolve <- function() s
    
    # Create and return the cache list of functions (set, get, setsolve, getsolve).
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    # Retreive the getsolve() function from the list.
    s <- x$getsolve()
    
    # Let us know if we're getting cached data (e.g. if s exists in the special list)
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # Get the data from the list.
    data <- x$get()
    
    # Solve the matrix inversion
    s <- solve(data, ...)
    
    # Set the cached value back into the list.
    x$setsolve(s)
    
    # Return the value.
    s
}
