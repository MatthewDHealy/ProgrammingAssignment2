
## AND THIS WORKS!
##
## at the end of this file is ACTUAL OUTPUT from running this through R, commented-out
##

## Two functions here.  First function uses a list of functions to
## create a simple class for an object that contains a matrix, its
## inverse, and getter/setter methods.  However this object DOES NOT
## KNOW how to compute is own inverse.  The inverse needs to be set
## with a call to its setInverse member function.

## The second function is a wrapper function that automatically
## does solve() and setInverse as needed.  It uses LEXICAL scoping
## rules to cache the matrix and its inverse, so that it will only
## call the setInverse method once unless the matrix has changed
## between calls

## As an experienced user of Perl, I find the <<- way of using lexical
## scoping annoying compared to the "my" keyword by which I cache stuff
## in Perl.  But this is an R programming class so I'll get off my
## soapbox now; linear models in Perl are a royal pain which is why
## I need to learn R now...

## first function basically just makes some closures to create and
## update our matrix object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # intialize with NULL for inverse, which will need to be computed
    # using solve() when it is needed.
    set <- function(y) {
        x <<- y
        # double-left arrow means that when we set new data for our
        # matrix object, we also null-out its inverse from the surrounding
        # scope
        # In Perl, I would do this with outer curlies to define a scope
        # and put the "my" statements declaring x and inv in the outer
        # scope while the "my" for y would be here.  Looks like R lacks
        # an equivalent of "use strict" or maybe Prof. Peng hasn't told
        # us about that yet?
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    # setter for inv member calls solve and uses double-left
    # arrow so it will update a copy of inv in the surrounding scope
    getInverse <- function() inv
    list(set = set, get = get,
	 setInverse = setInverse,
	 getInverse = getInverse
	 )
}


## now here is the wrapper that uses LEXICAL scoping rules to cache
## the matrix inversion results
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## first we see whether this matrix object already
	## knows its own inverse from a previous call
	inv <- x$getInverse()
	if (!is.null(inv)) {
           message("we find the cached inverse")
	   return(inv)
	}
	## If we get here then we need to compute and cache inverse
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	return(inv)
}



## here is actual output:
##
## > foo$getInverse()
## NULL
## > cacheSolve(foo)
##             [,1]        [,2]       [,3]       [,4]       [,5]
## [1,]  0.26466172 -0.17528879  0.1551746  0.5182705 -0.2115392
## [2,] -0.03481191 -0.80488945 -0.7686520 -0.6020167  0.3264827
## [3,] -0.30253975 -0.13616874  0.2092661 -0.2729333 -0.1938857
## [4,] -0.45351408 -0.03752579 -0.1767280 -0.1256793  0.1374676
## [5,] -0.23190210  0.29971997  0.8136014 -0.2704425  0.6665322
## > cacheSolve(foo)
## we find the cached inverse
##             [,1]        [,2]       [,3]       [,4]       [,5]
## [1,]  0.26466172 -0.17528879  0.1551746  0.5182705 -0.2115392
## [2,] -0.03481191 -0.80488945 -0.7686520 -0.6020167  0.3264827
## [3,] -0.30253975 -0.13616874  0.2092661 -0.2729333 -0.1938857
## [4,] -0.45351408 -0.03752579 -0.1767280 -0.1256793  0.1374676
## [5,] -0.23190210  0.29971997  0.8136014 -0.2704425  0.6665322
## >
##  

