makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL					#inverse matrix initiliazation 
	set <- function(y) {
		x <<- y
            inv <<- NULL
	}
	get <- function() x				#get matrix x 
	setInv <- function(inverse)inv <<- inverse
	getInv <- function()inv				#get INVERSE matrix  
	list(	set = set, get = get, 
		setInv = setInv,
		getInv = getInv)
}

cacheSolve <- function(x, ...) {					#get cache 
	inv <- x$getInv()
	if(!is.null(inv)) {				#checking if inverse is not NULL
		message("getting cached data")		#return inverse if exists
		return(inv)
       }
	 matrixx <- x$get()				#calculate inverse
       inv <- solve(matrixx, ...)
       x$setInv(inv)
       inv
}
