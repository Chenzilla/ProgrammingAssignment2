##These two functions work to cache the inverse of a given matrix, and returns the cached matrix if available.
##If not, the inverse is calculated.

## makeCacheMatrix caches the inverse matrix calculated from a given matrix

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse) i<<-inverse
    getInverse<-function()i
    list(set=set, get=get,
         setmean=setmean,
         getmean=getmean)
}


## Returns cached matrix if available or calculates it if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- set(data, ...)
    x$setInverse(i)
    i
}