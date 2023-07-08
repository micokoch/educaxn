# Week 2 class exercises
# From lecture
library(UsingR)
set.seed(1234)

# Mean using matrices
y <- father.son$sheight
print(mean(y))

N <- length(y)
Y <- matrix(y,N,1)
A <- matrix(1,N,1)
barY <- t(A) %*% Y / N ### equivalent to
barY <- crossprod(A,Y)/N
print(barY)

data("father.son",package="UsingR")

# Variance using matrices
r <- y - barY
crossprod(r)/N
# Which is equivalent to:
var(y) * (N-1)/N

# Minimize Residual Sum of Squares (RSS)
# Beta hat = (X^T X)^-1 X^T Y
x <- father.son$fheight
y <- father.son$sheight
X <- cbind(1,x)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
### or
betahat2 <- solve(crossprod(X)) %*% crossprod(X,y)


## Matrix algebra practice I
g <- 9.8
n <- 25
tt <- seq(0,3.4,len=n)
f <- 56.67 - 0*tt - 0.5*g*tt^2
y <- f + rnorm(n, sd=1)

plot(tt, y, xlab = "Time in seconds", ylab = "Distance in meters")
lines(tt,f,col=2)

rss <- function(Beta0,Beta1,Beta2){
  r <- y - (Beta0+Beta1*tt+Beta2*tt^2)
  sum(r^2)
}

Beta2s <- seq(-10,0,len=100)
RSS <- sapply(Beta2s,rss,Beta0=55,Beta1=0)
plot(Beta2s, RSS, type = "l")
RSS2 <- sapply(Beta2s,rss,Beta0=65,Beta1=0)
lines(Beta2s,RSS2,type = "l",col=3)

tt2 <- tt^2
fit <- lm(y~tt+tt2)
summary(fit)

## Matrix algebra practice II
X <- cbind(rep(1,length(tt)),tt,tt2)
head(X)

Beta <- matrix(c(55,0,5),3,1)

r <- y - X %*% Beta
RSS3 <- t(r) %*% r
RSS4 <- crossprod(r)
rss(55,0,5)

betahat3 <- solve( t(X) %*% X) %*% t(X) %*% y
betahat4 <- solve(crossprod(X)) %*% crossprod(X,y)

# solve() is unstable, so a solution is a QR decomposition

QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
backsolve(R,crossprod(Q,y)) # backsolve is more stable

###
# Matrix algebra example exercises
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

beta <- c(5, 2)

y <- X %*% beta
# Answer
fitted = X %*% beta
fitted[ 1:2, ]

# Answer
fitted = X %*% beta
fitted[ 3:4, ]

###
# Inference Review Exercises
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# Monte Carlo simulation
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)

# Model: y = b0 + b1 t + b2 t^2 + e

X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

# 1. What is the LSE of g (g=-2*b2)
# r <- y - X %*% Beta
# RSS3 <- t(r) %*% r
# RSS4 <- crossprod(r)
# betahat4 <- solve(crossprod(X)) %*% crossprod(X,y)
betahats <- A %*% y
(ghat <- -2*betahats[3])
# A%*%y gives us the LSE for all three coefficients

# 2. Set the seed to 1, then use the code above in conjunction with the function 
# replicate() to generate 100,000 Monte Carlo simulated datasets. For each dataset 
# compute an estimate of g (remember to multiply by -2).
# What is the standard deviation of this estimate?:

set.seed(1)
n = 25
wk2ex2 <- function(n){
  g = 9.8
  h0 = 56.67
  v0 = 0
  tt = seq(0,3.4,len=n)
  y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X)
  betahats <- A %*% y
  ghat <- -2*betahats[3]
  ghat
}
ghunthous <- replicate(100000, wk2ex2(n))
sd(ghunthous)
# [1] 0.429747

# Official answer
set.seed(1)
B = 100000
g = 9.8 ## meters per second
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

betahat = replicate(B,{
  y = 56.67  - 0.5*g*tt^2 + rnorm(n,sd=1)
  betahats = -2*A%*%y
  return(betahats[3])
})
sqrt(mean( (betahat-mean(betahat) )^2))
# [1] 0.4297449



