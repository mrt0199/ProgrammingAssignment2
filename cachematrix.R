# This is a modification of the example provided, where a function was created to calculate the mean of a vector and cache it for later retrieval.  
#the structure of the example program is modified to instead compute the inverse value of a matrix and cache it.  Doing this allows the cached value to 
#be retrieved instead of being recomputed.  The solve() function is what calculates the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) { #makeCacheMatrix is defined such that it is a function of the argument x, which is a matrix.  The purpose
        #of this function is to create the matrix x and set it up to cache its inverse value once calculated.  That way, each time the need arises to calculate the inverse
        # of x, so long as x has not changed, the value is taken from cache and is not recalculated.
        inv <- NULL # inv is defined as an object and assigned a value of NULL.  This object will receive the 
        # cached value of the inverse of x when calculated by the cachesolve funtion below.
        set <- function(y) {  #.  The first step is to assign the paramaters of a matrix to y.  To test this, I will create assign the parameters of a matrix named
                #b and c, which will be passed to y when this function is run.
                x <<- y # the value y is assigned to x, but the "<<-" operator causes a search for a definition of x in the parent environment
                inv <<- NULL# a null value is assigned to inv, and the definition of inv is obtained from the parent environment.
        }
        get <- function() x  #the second step is get the parameters of the matrix that were just set
        setInverse <- function(inverse) inv <<- inverse  #the third step is to set the inverse of x and store it in inv
        getInverse <- function() inv  #the fourth step is to get the inverse value of x
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#now the structure is in place to store the values and inverse values of a matrix, the next step is to actually calculate the inverse
cachesolve <- function(x, ...) {#the purpose of this function is to calculate the inverse of a function and store the value in inv, and if the funcion is called again,
        #to provide the cached value instead of recalculating it again
        inv <- x$getInverse() 
        if(!is.null(inv)) {#if there is already a value assigned to inv, provide that value and display a message that the data is cached, otherwise, calculate the 
                #inverse of the matrix, as shown in the next 2 lines of code.
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)#calculate the inverse of the matrix with the solve function
        x$setInverse(inv) #assign this value to inv
        inv #display the value
}
b<-matrix(1:4, 2, 2)  #to test this, I created a matrix called b
b # display b
a<-makeCacheMatrix(b) #call the function that will set b
cachesolve(a) #calculate the inverse of b and display it
cachesolve(a) #call the function again, see if it calls a cached value and displays the message
c<-matrix(2:5, 2,2) #create another function 
c #display c
a<-makeCacheMatrix(c)#call the function that will set c
cachesolve(a) #calculate the inverse of c and display it
cachesolve(a) #call the funcaiton again, see if it calls a cached value and display the message