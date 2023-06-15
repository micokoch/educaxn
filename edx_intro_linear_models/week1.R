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

