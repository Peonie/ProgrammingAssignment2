## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){ # one argument 'x' which is a matrix
        inv <- NULL #create an object for the inverse matrix
        set <- function(y){ #create nested function 1. which sets the values of x and m in the parent environment of this function 
                x <<- y # <<- assignment operator used so objects are available in parent environment 
                m <<- NULL
        }
        get <- function() x #create nested function 2. which takes the matrix 'x' or gets the matrix/receives the matrix data
        #no curly braces used as one line function
        setinverse <- function(inverse) inv <<- inverse #create nested function 3. which defines the setter for the inverse 
        getinverse <- function() inv #create nested function 4. which defines the getter for the inverse
        list(set = set, get = get, # creates a list of the four nested functions created so these can be accessed in cacheSolve
             getinverse = getinverse,
             setinverse = setinverse)
}


## Write a short comment describing this function

#cacheSolve () - a function that calculates the inverse of cached matrix
cacheSolve <- function(x, ...){
        inv <- x$getinverse() # function tries to receive the inverse of a matrix already - if calculated
        if(!is.null(inv)){ # if there is an inverse then message appears saying 'getting the cached data' i.e. retrieving the inverse
                message("getting cached data")
                return(inv) #the funciont hen returns the inverse 
        }
        data <- x$get() #if no inverse avialable forcthe matrix a new inverse is calculated and this line gets the matrix
        inv <- solve(data, ...)# this line computes the inverse using the solve() function
        x$setinverse(inv) #this line then re-sets the inverse matrix
        inv# this line prints the new inverse
}
