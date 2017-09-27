for(i in 1:5) print(1:i)

for(n in c(2,5,10,20,50)){
  print(n)
}

for(i in 1:5) 
  print(1:i)

for(i in 1:5)
  print(i)


x = 0
n = 0
for(i in top_stations_by_start) {
  print(n)
  n = n + 1
}
  
mytrans <- function(x) { 
  if (!is.matrix(x)) {
    warning("argument is not a matrix: returning NA")
    return(NA_real_)
  }
  y <- matrix(0, nrow=ncol(x), ncol=nrow(x)) 
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j] 
    }
  }
  return(y)
}