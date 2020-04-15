
library(pracma) #Need this

newton2 <- function(f, guess, itmax) {
  #Itmax is max # of iterations
  x_prev <- guess #initial guess
  int <- 1
  while (int < itmax) {
    J <- jacobian(f, x_prev) #Uses jacobian function in pracma (which is a Matlab replicator in R)
    x <- x_prev - inv(J) %*% t(rbind(f(x_prev))) #Matrix function
    
    x_prev <- x
    int <- int + 1
    x
  }
  return(x)
}

