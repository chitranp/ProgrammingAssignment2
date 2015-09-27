
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
	#essentially, it is a list containing function to
	#1.Set the value of the matrix
	#2.Get the value of the matrix
	#3.Set the inverse value of the matrix
	#4.Get the inverse value of the matrix

makeCacheMatrix<-function (x=matrix()) {
	m<- NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function()m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed)
#the function retrieves the inverse from the cache.
cacheSolve<-function(x, ...) {
	m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m
}

#example
dot<-matrix(as.numeric(c(1,2,3,8,9,12,24,56,37)),3,3)
chi<-makeCacheMatrix(dot)
cacheSolve(chi)
cacheSolve(chi)
