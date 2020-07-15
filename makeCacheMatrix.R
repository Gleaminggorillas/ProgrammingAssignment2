#NOTE: this significantly similar to the appropriate assignment example by JHU.
# I have essentially comprehended it, documented, and changed a few
# variables including: 
# the argument 'vector()' to 'matrix()' in line 18, along with other
# changes to variable names for sake of clarity. If there is 
# a problem please let me know, via a message on GitHub, or e-mail.
#
#==========================================================================
#
# Calculating inversions of matrices takes a hell of a lot of computation in R. 
# Prevent unnecessary computation by storing and referencing a "getter-setter
# module" of previously calculated matrices and their inversions.
#
# Formal terminology; Mutators and Accessors. A mutator ("setter") provides
# the resulting function of an argument. An accessor ("getter") retrieves data
# within the object.
#
# ** This will only work for non-singular, symmetrical matrices 2x2, 3x3, 4x4, etc.

#create a function to take a matrix
makeCacheMatrix <- function(x = matrix()){
  
  #set empty cache.
  cache <- NULL
  
  #child function of makeCacheMatrix
  #now makeCacheMatrix2$set will clear prior cachecalcs,
  # and send the argument to parent, which stores the matrices.
  
  set <- function(xchild) {
    x <<- xchild
    cache <<- NULL
  }
  
  #for later, this is the function which stores the matrix
  get <- function() x
  
  #
  setInvMat <- function(solve) cache <<- solve
  
  #for later, this function accesses the inverted matrix stored in cache without
  #calculating the matrix
  getInvMat <- function() cache
  
  #this list allows CacheSolve to access the cache of 'makeCacheMatrix'
  list(set = set, get = get,
       setInvMat = setInvMat, getInvMat = getInvMat)
  
}

# Make a second function, 'CacheSolve' to check input against 'makeCacheMatrix',
# and calculate inversion if not already stored in 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
  
  #check cache
  cache <- x$getInvMat()
  
  #if info is in the cache, it will return the cache.
    if(!is.null(cache)){
      message('retrieving from cache')
      return(cache)
    }  
  
  #if memory is empty
  mat <- x$get()
  cache <- solve(mat, ...)
  x$setInvMat(mat)
  return(mat)
    
  
  
}
