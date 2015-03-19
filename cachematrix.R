
## 18 Mar., 2015
## Assignment 2, prepared by Chuck Hill
# 
##  Values below are for matrix inverse, calculated by running 
#   cacheSolve(x) from console. Return value =
#       [,1] [,2]
# [1,]  -2  1.5
# [2,]   1 -0.5
#
M <- matrix(1:4, 2, 2)  # simple matrix to load and test code
x <-makeCacheMatrix(M) # run first function to load the matirix into cache memory.

cacheSolve(x) # Run cacheSolve to calculate inverse after checking the value of m 
# which will be NULL on the first run and then the same as listed on lines 6-8 above.
# On the second run, you get the message "getting cached data" before the output.

# makeCacheMatrix will store the matrix in the global environment memory. 

makeCacheMatrix <- function(x = matrix()) { # Matrix "M" is the variable passed here as x.
        
        # The first time run, m is assigned to NULL
        m <- NULL
        set <- function(y) {
                x <<- y         # y is the value passed when calling x$set and
                                # is stored in the parent environment.
                m <<- NULL
        }
        get <- function() x     # returns the value of x within its lexical scope
                                # (when the function was defined), when called by x$get.
        
        # The last step performed by cacheSolve (below) is to call $setinverse, which stores 
        # the inverted matrix m in in cache memory here again as m.
        setinverse <- function(inverse) {
                m <<- inverse
        }
        # called by cacheSolve (below) as $getinverse. Returns inverse of input matrix M, 
        # which is NULL on the first pass of running makeCacheMatrix.
        getinverse <- function() {
         m       
        }
        
        # list of functions that are passed to cacheSolve:
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)
                 
} # End of MakeCacheMatrix

# cachesolve will solve for the inverse of a matrix. If a subsquent run of another 
# matrix is an identical matrix, then the value stored in x <<- y will be re-used and you see
# the message "getting cached data" at the console.

cacheSolve <- function(x, ...) {
        # The value of x at this point is not a variable, but a list of functions:
        # $set; 
        # $get;
        # $setinverse
        # $getinverse
        
        # Calls $getinverse in cacheMatrix above:
        m <- x$getinverse()
        # The value of m is the inverse of matrix M, or NULL on the first pass
        
        # If inverse is already calculated then m is not NULL and goes into if condition next.
        
        # If inverse stored is already calculated, then value of m is returned here:
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                        print(m) 
                } # end if 
        
        # Calls x$get (which is the original matrix M) from above and stores the 
        # matrix M in variable data:
        data <- x$get()

        # Calculate the inverse of matrix M stored in data:
        m <- solve(data, ...) 
       
        # The inverted matrix m will be stored in cache memory by x$setinverse:
        x$setinverse(m)
        
        # Return the inverted matrix to the console.
        m
} # End of cacheSolve
        