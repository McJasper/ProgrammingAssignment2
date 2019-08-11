## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix constructs a matrix object and has the handler
##functions necessary to manipulate its state.  The matrix object
##has fields for holding a matrix and it's inverse, as well as the
##helper functions necessary to access and alter the fields.

makeCacheMatrix <- function(x = matrix()) {
    #Create a field to hold the matrix inverse
    cachedInverse<-NULL
    
    #Set function which sets the value of the matrix and resets the matrix inverse
    set<- function(y){
        x<<-y
        cachedInverse <<- NULL
    }

    #get function which returns the matrix
    get<-function() x
    
     #Set for matrix inverse
    setInverse<-function(invertedMatrix){
        cachedInverse <<- invertedMatrix
    }
    
    #gets matrix inverse and returns it
    getInverse<-function() cachedInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function

##cacheSolve is a helper function that checks a makeCacheMatrix object
##for an existing inverse.  If one exists, it returns the cached inverse.
##If not, it calculates the inverse using solve(...), caches the result, 
## and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #Assumes x is invertable without checks...
    #First, checks to see if the matrix has a cached inverse

    xInverse<-x$getInverse()
    
    #If no it calculates the inverse via solve() and sets the inverse field
    if(is.null(xInverse)){
        xInverse<-solve(x$get(), ...)
        x$setInverse(xInverse)
        print("Calculating matrix inverse...")
    }
    #If yes, it extracts the inverse and returns it
    else{
        print("Getting cached matrix inverse...")
    }
    return(xInverse)
}

# testMatrix <- makeCacheMatrix()
# testMatrix$set(matrix(c(1,0,0,0,1,0,0,0,1), ncol = 3))
# testMatrix$get()
# 
# cacheSolve(testMatrix)
# # Returns calculated matrix
# cacheSolve(testMatrix)
# # Returns cached matrix
