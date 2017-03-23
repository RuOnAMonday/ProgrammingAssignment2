## makeCacheMatrix takes a matrix and caches it's inverse. The cacheSolve function actually 
## calculates the inverse and returns it to the makeCacheMatrix function.
## If the inverse has already been calculated, then the inverse will be retrived from cache


## makeCacheMatrix takes a matrix and caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setmyinverse <- function(myinverse) inv <<- myinverse
  getmyinverse <- function() inv
  list(set = set, get = get,
       setmyinverse = setmyinverse,
       getmyinverse = getmyinverse)
}


##The cacheSolve function uses the matrix passed to makeCacheMatrix and 
## calculates the inverse and returns it to the makeCacheMatrix function.
## If the inverse has already been calculated, then the inverse will be retrived from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmyinverse()

    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setmyinverse(inv)
  inv
}
