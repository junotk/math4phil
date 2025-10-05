logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}



x1 = logistic.map(4, 0.1, 50, 50)
x2 = logistic.map(4, 0.11, 50, 50)
t = 0:(length(x1)-1)
plot(t, x1, type="l", xlab="t", ylab="x", col="Blue")
lines(x=t, y=x2, col="Green")
