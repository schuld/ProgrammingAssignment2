## Testcase for hw2

## The following function is from:
## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

## Outline of testcase:
##
## Part I
## - Create matrix m
## - Create cached matrix x
## - Solve for the inverse of matrix x, twice
## - Compare the results to ensure both solutions are equal,
##       and that the first time it is calculated and the second
##       time it is retrieved from the cache without calculation
## - Re-invert the inverted matrix, twice, and compare to the
##       original matrix
##
## Part II
## - Create a much larger matrix and confirm that there is a
##       time savings by using the cached value.

message("Testcase for cacheSolve():")
message("")
message("PART I")
message("")
m <- matrix(c(-1,-2,1,1),2,2)
x <- makeCacheMatrix(m)
message("Original matrix:")
print(x$get())
message("")

message("Solve inverse matrix - uncached")
inv <- cacheSolve(x)
print(inv)
message("")

message("Solve again - should retrieve cached value")
inv2 <- cacheSolve(x)
print(inv2)
message("")

message("Confirm if cached value is identical to calculated value")
print(matequal(inv,inv2))
message("")

message("Re-invert the matrix, compare to original matrix")
x$set(inv)
print(x$get())
inv <- cacheSolve(x)
print(inv)
print(matequal(inv,m))
message("")

message("Re-invert again, verify cached, and compare")
inv2 <- cacheSolve(x)
print(matequal(inv,inv2))
message("")

message("-----------------------------------------------------")
message("")
message("PART II")
message("")

## The following was inspired by a response to the Forum
## subject "Easy Inverse Test" in the rprog-013 forums:

## Calculate nXn inverse and compare runtime to cached lookup time
n <- 2048
mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
matCached <- makeCacheMatrix(mat)
time1 <- system.time(matSolved1 <- cacheSolve(matCached))
time2 <- system.time(matSolved2 <- cacheSolve(matCached))
print(time1)
print(time2)
if(time1["user.self"] < time2["user.self"])
  message("Solve time is less than cache time")
message("")
message("End of Tests")