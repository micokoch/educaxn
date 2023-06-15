library(UsingR)
data("father.son",package="UsingR")

# 1. What is the average height of the sons (don't round off)?
head(father.son)
summary(father.son)
mean(father.son$sheight)
# 68.68407

# 2. What is the mean of the son heights for fathers that have a height 
# of 71 inches (don't round off your answer)?
mean(father.son$sheight[round(father.son$fheight) == 71])
# 70.54082

# More code
RNGkind()
# From video
y=father.son$sheight
head(y)
x=father.son$fheight
# From exercise
X = matrix(1:1000,100,10)

# 3. What is the entry in row 25, column 3?
X[25,3]

# 4. Using the function cbind(), create a 10 x 5 matrix with first column x=1:10. 
# Then use 2*x, 3*x, 4*x and 5*x respectively in columns 2 through 5.
# What is the sum of the elements of the 7th row?

ex2c1 <- 1:10
matex2 <- cbind(ex2c1, ex2c1*2, ex2c1*3, ex2c1*4, ex2c1*5)
sum(matex2[7,])
# 105

# 5. Matrix question
View(matrix(1:60,20,3,byrow = T))

# 6. R question
seq(10,1,-2)

# More code
# From videao
X <- matrix(1:12,4,3)
print(X)
a <- 2
print(a*X)
(X <- matrix(1:15,5,3))
t(X)
# Equations
# a+b+c=6;3a-2b+c=2;2a+b-c=1
(X <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3))
beta <- c(3,2,1)
X%*%beta ## is not (6 2 1)
# Identity matrix
diag(5)
# Solving the equation (solve is inverse matrix)
(Y <- matrix(c(6,2,1),3,1))
(beta <- solve(X)%*%Y)
X%*%beta

# 6. Solve equations:
# 3a+4b-5c+d=10;2a+2b+2c-d=5;a-b+5c-5d=7;5a+d+4
# What is the solution for c?
(X <- matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1),4,4,byrow=T))
(Y <- matrix(c(10,5,7,4),4,1))
(beta <- solve(X)%*%Y)
X%*%beta
# -0.8849558

# 7. 
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
(ab <- a%*%b)
ab[3,2]
# 113

# 8. Multiply the 3rd row of a with the 2nd column of b, using the element-wise vector 
# multiplication with *.
# What is the sum of the elements in the resulting vector?
sum(a[3,]*b[,2])
# 113