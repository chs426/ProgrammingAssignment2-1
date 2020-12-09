#The following function create matrix object that will cache its inverse.

makeVector <- function(x = numeric()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setmean <- function(mean) m <<- mean
       getmean <- function() m
       list(set = set, get = get,
            setmean = setmean,
            getmean = getmean)
}

#This function computes the inverse of the matrix makeCacheMatrix
#If the inverse has ben already calculated without any change, the inverse will be retrieved from the cache.
cachemean <- function(x, ...) {
       m <- x$getmean()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- mean(data, ...)
       x$setmean(m)
       m
}
