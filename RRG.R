rm(list=ls())


library(igraph)
library(MASS)

writeRandMat <- function(n)
{
  res <- matrix(0,100*n,n)
  for (i in 1:100)
  {
    res[((i-1)*n+1):(i*n),1:n] = as.matrix(get.adjacency(k.regular.game(n,3,T,F)))
  }
  print(res)
  write.matrix(res,paste('g',n,sep=""))
}

for (i in (6:20))
{
  writeRandMat(i)
}
