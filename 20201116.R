alpha <- 0.5
n<-1000
set.seed(0)
x<- c()
for (i in c(1:n)){
  U1 <- runif(1)
  U2 <- runif(1)
  x <- c(x,ifelse(U1 < 0.5,floor(10*U2)+1,floor(5*U2)+6))
}

prop.table(table(x))

pj <- c(rep(0.05,5),rep(0.15,5))
chisq.test(table(x),p = pj)

#cotinuous random variables
k<-2
Fx<-function(x,k=k) x^k
curve(Fx(x,k=k),from = 0,to = 1)

G <- function(u,k) u^(1/k)
Y <- G(runif(n), k=k)
plot(density(Y), main = "", xlab="Y")



integrate(f=function(x, k=1) 1-x^3, lower = 0 , upper = 1)

n <- 1000
lambda <- 0.2
Y <- -(1/lambda)*log(1-runif(n))
plot(density(Y, from =0),xlab ="Y",main="")
curve(dexp(x,rate=lambda),from = 0,col="red",add=T)
legend("topright",c("simulation","exact"),col = c(1,2), lty=1)

 
B <- 5000
lambda <-2
n<-5
x<-c()
for (i in 1:B){
  x <-c(x, -(1/lambda)*log(prod(runif(n))))
}
plot(density(x))
curve(dgamma(x,shape = n,scale = 1/lambda),col="red")


n <- 1000
X<-c()
num.c<-c()
set.seed(123)
for (i in 1:n){
  k <- 1
  while (TRUE) {
    U1<-runif(1)
    U2<-runif(1)
    if ( U2 < (256/27)*U1*(1-U1)^3 ) {
      X[i] <- U1
      num.c[i] <- k # number of steps until success
      break()
    } else {
      k <- k+1
    }
  }
}


set.seed(123)
n <- 5000
X<-c()
num.c<-c()
set.seed(123)
for (i in 1:n){
  k <- 1
  while (TRUE) {
    U1 <- runif(1)
    Y <- -3/2*log(U1)
    U2 <- runif(1)
    if ( U2 < (2*exp(1)/3)^(1/2)*Y^(1/2)*(exp(-Y/3)) ) {
      X[i] <- Y
      num.c[i] <- k # number of steps until success
      break()
    } else {
      k <- k+1
    }
  }
}


