# Hash de4d307acbc92e956377a36409952a6ff8f65b56

# Creates matrix inverse and stores it in a casche
makeCacheMatrix <- function(x = matrix()) {
        # creates a placeholder
        xinv <- NULL 

        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        set <- function(y) {
                x <<- y
                # creates placeholder for inverted matrix
                xinv <<- NULL 
        }
        # return the input matrix
        get <- function() x 
        # set the inversed matrix
        setInv <- function(inv) xinv <<- inv 
        # return the inversed matrix
        getInv <- function() xinv 
        # return a list that contains these functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

# retrieves a cashedmatrix or creates one if missing.
cacheSolve <- function(x, ...) {
        m <- x$getInv() # get the inversed matrix from object x
        # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
        if(!is.null(m)) { # if the inversion result is there
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get() # if not, we do x$get to get the matrix object
        #calculate matrix
        m <- solve(data)
        x$setInv(m)
        return(m)
}

# Test
# generate a random square, non-singular matrix - not integers
test <- matrix(runif(9,5,100),3,3)
# test the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function
testInv <- cacheSolve(testCached)

