makeCacheMatrix <- function(x = matrix()){
      if(det(x)==0){
            message("matrix not inversible")
            return(NULL)
      }
      inv = NULL
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function() x
      setinv <- function(mat) inv<<-mat
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)      
}
cacheSolve <- function(x,...){
      inv<-x$getinv()
      if(!is.null(inv)){
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
