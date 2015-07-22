## Computes the inverse of a matrix and caches it to limit time-consuming computations 



makeCacheMatrix <- function(x = matrix()) {
## creates a special "matrix" which is a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse
	I<-NULL
        set<-function(y){
                x<<-y
                I<<-NULL
        }
        get<-function() x
        setInverse<-function(solve) I<<-solve
        getInverse<-function() I
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}



cacheSolve <- function(x, ...) {
##calculates the inverse of the special "matrix" created with the above function; first checking if the inverse has already been calculated. If so, the inverse is obtained from the cache and the computation is cached, otherwise the inverse is calculated and set in the cache
	I<-x$getInverse()
        if(!is.null(I)){
                message("getting cached data")
                return (I)
        }
        data<-x$get()
        I<-solve(data, ...)
        x$setInverse(I)
        I
} 

