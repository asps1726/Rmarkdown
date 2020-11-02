A <- sample(x=0:3,10000,replace=TRUE, prob= c(1/8,3/8,3/8,1/8))
prop.table(table(A))

B<-runif(1000)
N<-10
C<-floor(B*N)+1
prop.table(table(C))


times <- 10000
bematrix <- c()
N_length <- 10
for (time in c(1:times)){
  X <- c(1:N_length)
  
  for (k in c(N_length:1)){
    I <- floor(runif(1)*k)+1
    temp_Xi <- X[I]
    X[I]<-X[k] ;X[k]<- temp_Xi
  }
  print(X)
  bematrix <- c(bematrix,X)
}
bematrix <- matrix(bematrix,nrow=times,ncol = N_length,byrow = TRUE)
bematrix
mean(bematrix[,1])

