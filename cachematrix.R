## This sets the initial variables and values for the function
makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list
  (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## This confirms the matrix has an inverse and gets it from cache
cacheSolve <- function(x, ...) 
  {
  i <- x$getinverse()
  if (!is.null(i)) 
    {
    message("Retrieving data from cache")
    return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }