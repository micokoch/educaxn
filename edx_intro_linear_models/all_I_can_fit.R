###### 3 hours to cram all the info left in course
library(UsingR)
set.seed(1234)

### Week 2 (cont'd)
# Standard Error
x =  father.son$fheight
beta =  c(34,0.5)
var(beta[1]+beta[2]*x)

y = h0 + v0*tt  - 0.5*g*tt^2 + rnorm(n,sd=1)
var(y)

# Standard Error Exercises
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef

# Exercise 1
fit = lm(y ~ x)
fit$fitted.values
# What is the sum of the squared residuals?
fit = lm(y ~ x)
SSR <- sum((y - fit$fitted.values)^2)
# 331.2952

# Exercise 2
# Our estimate of sigma^2 will be the sum of squared residuals divided by (N - p), 
# the sample size minus the number of terms in the model. Since we have a sample 
# of 50 and 2 terms in the model (an intercept and a slope), our estimate of sigma^2
# will be the sum of squared residuals divided by 48. Save this to a variable sigma2:
sigma2 = SSR / 48
# where SSR is the answer to the previous question.

# Form the design matrix X (note: use a capital X!). This can be done by combining a 
# column of 1's with a column of x the father's heights.
X = cbind(rep(1,N), x)
# Now calculate (X^T X)^(-1), the inverse of X transpose times X. Use the solve() function 
# for the inverse and t() for the transpose. What is the element in the first row, 
# first column?
solve(t(X) %*% X)
# 14.5639039

# Exercise 3
# Now we are one step away from the standard error of beta-hat. Take the diagonals from 
# the (X^T X)^(-1) matrix above, using the diag() function. Now multiply our estimate of 
# sigma^2 and the diagonals of this matrix. This is the estimated variance of beta-hat, 
# so take the square root of this. You should end up with two numbers, the standard error 
# for the intercept and the standard error for the slope.

# What is the standard error for the slope?
fit = lm(y ~ x)
sigma2 = sum((y - fit$fitted.values)^2) / (N - 2)
sqrt(sigma2 * diag(solve(t(X) %*% X)))
# Answer: 0.1479065
#                     x 
# 10.0259579  0.1479065

# Compare this value with the value you estimated using Monte Carlo in the previous assessment. 
# It will not be the same, because we are only estimating the standard error given a particular 
# sample of 50 (which we obtained with set.seed(1)).

# Note that the standard error estimate is also printed in the second column of:
summary(fit)

# Quiz
# Suppose now we are comparing two treatments B and C to a control group A, each with two samples. 
# This design can be represented with a model matrix like so:
  
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
# which results in a matrix that looks like
# a 1 0 0
# a 1 0 0
# b 1 1 0
# b 1 1 0
# c 1 0 1
# c 1 0 1
# Suppose that the fitted values for the linear model are given by:
beta <- c(10,3,-3)
# Make sure that you are using the correct random number generator (RNG) settings by calling 
# the following command:
RNGkind("Mersenne-Twister", "Inversion", "Rejection")


### Week 3
## Exercises
# Suppose we have an experiment with the following design: on three different days, we perform 
# an experiment with two treated and two control samples. We then measure some outcome y_i, and 
# we want to test the effect of condition, while controlling for whatever differences might 
# have occured due to the the different day (maybe the temperature in the lab affects the 
# measuring device). Assume that the true condition effect is the same for each day (no 
# interaction between condition and day). We then define factors in R for day and for condition.
# day: A B C
# condition: --------------
#   treated | 2 2 2
# control | 2 2 2

# Given the factors we have defined above, and not defining any new ones, which of the following 
# R formula will produce a design matrix (model matrix) that let's us analyze the effect of 
# condition, controlling for the different days:
# Answer: ~ day + condition
# Explanation:
# Using the ~ and the names for the two variables we want in the model will produce a design 
# matrix controlling for all levels of day and all levels of condition, so ~ day + condition. 
# We do not use the levels A,B,C etc in the design formula.

## More exercises
# In the t-test, the denominator of the t-value is the standard error of the difference. 
# The t-test formula for the standard error of the difference, if we assume equal variance 
# in the two groups is:
# SE = sqrt(var(diff))
# var(diff) = (1/nx + 1/ny) (sum {(x_i - mu_x)^2} + sum{(y_i - mu_y)^2}) / (nx + ny - 2)
# where nx is the number of samples in the first group and ny is the number of samples in the 
# second group.
# If we look carefully, the second part of this equation is the sum of squared residuals, 
# divided by (N - 2).
# So all that is left to show is that:
# where nx is the number of samples in the first group and ny is the number of samples in the 
# second group.
# If we look carefully, the second part of this equation is the sum of squared residuals, 
# divided by (N - 2).
# So all that is left to show is that:
# ( (X^T X)^-1 )[2,2] = (1/nx + 1/ny)

# ...where [2,2] indicates the 2nd row, 2nd column, with X as the design matrix of a linear 
# model of two groups.

# LM Exercise 1
# You can make a design matrix X for a two group comparison either using model.matrix() or 
# simply with:
X = cbind(rep(1,nx + ny), 
          rep(c(0,1), 
              c(nx, ny)))

# For a comparison of two groups, where the first group has nx=5 samples, and the second group 
# has ny=7 samples, what is the element in the 1st row and 1st column of X^T X?
XtX = t(X) %*% X 
XtX[ 1,1 ]
# Answer: 12

# LM Exercise 2
# What are all the other elements of X^T X?
t(X) %*% X
# Answer: 7


# Now we just need to invert the matrix to obtain  (X^T X)^-1
# The formula for matrix inversion for a 2x2 matrix is:
# [a b] ^ -1                  [d -b]
# [c d]       = 1 / (ad - bc) [-c a]
# The element of the inverse in the 2nd row and the 2nd column is the element which will be 
# used to calculate the standard error of the second coefficient of the linear model. This is:
# a / (ad - bc)
# And for our two group comparison, we saw that a = nx + ny and the b = c = d = ny. 
# So it follows that this element is:
# (nx + ny) / ((nx + ny) ny - ny ny)
# which simplifies to:
# (nx + ny) / (nx * ny)
# which simplifies to:
# (1/ny + 1/nx)


### Week 4
## Exercises
# Remember, you can check the book page for contrasts: 
# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html
# Suppose we have an experiment with two species A and B, and two conditions: control and treated.
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
# And we will use a formula of ~ species + condition.
# The model matrix is then:
model.matrix(~ species + condition)

# Contrasts Exercises #1
# Suppose we want to build a contrast of coefficients for the above experimental design.
# You can either figure this question out through logic, by looking at the design matrix, 
# or using the contrast() function from the contrast library. If you have not done so already, 
# you should download the contrast library. The contrast vector is returned as contrast()$X.

# What should the contrast vector be, for the contrast of (species=B and condition=control) vs 
# (species=A and condition=treatment)? Assume that the beta vector from the model fit by R is: 
# Intercept, speciesB, conditiontreated.
library(contrast)
y = rnorm(4)
fit = lm(y ~ species + condition)
contrast(fit, list(species="B",condition="control"), list(species="A",condition="treated"))$X
# Answer: 0 1-1
# Explanation
# As you want to compare species B vs A and condition control vs treated, you want +1 for 
# the speciesB coefficient and -1 for the conditiontreated coefficient.
# Another way to find this would be by generating some random y, fitting a model, and 
# using contrast():

# Contrasts Exercises #2
# Load the spider dataset like this:
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

# Run the code from the Rmd script of the spider dataset. Suppose we build a model using 
# two variables: ~ type + leg.
# What is the t-value for the contrast of leg pair L4 vs leg pair L2?
# Explanation
# Use the lines of code from the Rmd script up to the definition of fitTL
contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))
# Answer: 2.45


### Interactions Exercises
# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
# Suppose that we notice that the within-group variances for the groups with smaller frictional 
# coefficients are generally smaller, and so we try to apply a transformation to the frictional 
# coefficients to make the within-group variances more constant.

# Add a new variable log2friction to the spider dataframe:
spider$log2friction <- log2(spider$friction)
# The 'Y' values now look like:
boxplot(log2friction ~ type*leg, data=spider)
# Run a linear model of log2friction with type, leg and interactions between type and leg.

# Interactions Exercises #1
# What is the t-value for the interaction of type push and leg L4? If this t-value is 
# sufficiently large, we would reject the null hypothesis that the push vs pull effect on 
# log2(friction) is the same in L4 as in L1.
fit = lm(log2friction ~ type + leg + type:leg, data=spider)
summary(fit)
# and read off the t-value for typepush:legL4.
# Answer: -3.689

# Interactions Exercises #2
# What is the F-value for all of the type:leg interaction terms, in an analysis of variance? 
# If this value is sufficiently large, we would reject the null hypothesis that the push 
# vs pull effect on log2(friction) is the same for all leg pairs.
fit = lm(log2friction ~ type + leg + type:leg, data=spider)
anova(fit)
# and read off the F-value for the type:leg row.
# Answer: 10.701

# Interactions Exercises #3
# What is the L2 vs L1 estimate in log2(friction) for the pull samples?
contrast(fit, list(type="pull",leg="L2"), list(type="pull",leg="L1"))
# which is simply the legL2 coefficient:
coef(fit)["legL2"]
# Answer: 0.3468125

# Interactions Exercises #4
# What is the L2 vs L1 estimate in log2(friction) for the push samples? Remember, 
# because of the interaction terms, this is not the same as the L2 vs L1 difference 
# for the pull samples. If you're not sure use the contrast() function. 
# Another hint: consider the arrows plot for the model with interactions.
contrast(fit, list(type="push",leg="L2"), list(type="push",leg="L1"))
# which is the legL2 coefficient plus the push:L2 interaction:
coef(fit)["legL2"] + coef(fit)["typepush:legL2"]
# Answer: 0.4464843


## Colinearity Exercises
# To answer the first question, consider the following design matrices:
# A
# 1 0 0 0
# 1 0 0 0
# 1 1 1 0
# 1 1 0 1
# 
# B
# 1 0 0 1
# 1 0 1 1
# 1 1 0 0
# 1 1 1 0
# 
# C
# 1 0 0 
# 1 1 2 
# 1 2 4 
# 1 3 6 
# 
# D
# 1 0 0 0 0
# 1 0 0 0 1
# 1 1 0 1 0
# 1 1 0 1 1
# 1 0 1 1 0
# 1 0 1 1 1
# 
# E
# 1 0 0 0
# 1 0 1 0
# 1 1 0 0
# 1 1 1 1
# 
# F
# 1 0 0 1
# 1 0 0 1
# 1 0 1 1
# 1 1 0 0
# 1 1 0 0
# 1 1 1 0

# Collinearity Exercises #1
# Which of the above design matrices does NOT have the problem of collinearity?
# Answer: E
# Explanation:
# You can check in R, the rank of the E matrix is equal to the number of columns, 
# so all of the columns are independent.
m = matrix(c(1,1,1,1,0,0,1,1,0,1,0,1,0,0,0,1),4,4)
qr(m)$rank


# Let's use the example from the lecture to visualize how there is not a single 
# best beta-hat, when the design matrix has collinearity of columns.
# An example can be made with:
sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))
# The model matrix can then be formed with:
X <- model.matrix( ~ sex + trt)
# And we can see that the number of independent columns is less than the number of columns of X:
qr(X)$rank
# Suppose we observe some outcome, Y. For simplicity we will use synthetic data:
Y <- 1:8
# Now, we will fix the value for two beta's and optimize the remaining betas. We will fix 
# beta_male and beta_D. And then we will find the optimal value for the remaining betas, 
# in terms of minimizing SUM(i=1 to n) of (y_i - x_i * Beta)^2.

# The optimal value for the other betas is the one that minimizes:
# SUM(i=1 to n) of ((y_i - x_(i,male) * Beta_male - x_(i, D) * Beta_D) - x_(i,R) * Beta_R)^2
# Where X_male is the male column of the design matrix, X_D is the D column, and X_R has the 
# remaining columns.

# So all we need to do is redefine Y as Y* = Y - X_male * Beta_male - X_D * Beta_D 
# and fit a linear model. The following line of code creates this variable Y*, 
# after fixing Beta_male to a value a, and Beta_D to a value b:
makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b
# Now we'll construct a function which, for a given value a and b, gives us back the 
# sum of squared residuals after fitting the other terms.
fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

# Collinearity Exercises #2
# What is the sum of squared residuals when the male coefficient is 1 and the D coefficient 
# is 2, and the other coefficients are fit using the linear model solution?
fitTheRest(1,2)
# Answer: 11

# We can apply our function fitTheRest to a grid of values for beta_male and beta_D, 
# using the expand.grid function in R. expand.grid takes two vectors and returns a matrix 
# with rows containing all possible combination. Try it out:
expand.grid(1:3,1:3)
# We can run fitTheRest() on a grid of values, using the following code (the Vectorize() 
# is necessary as outer() requires only vectorized functions):
betas = expand.grid(-2:8,-2:8)
rss = apply(betas,1,function(x) fitTheRest(x[1],x[2]))

# Collinearity Exercises #3
# Which of the following pairs of values minimizes the RSS?
# (a) 8, -2; (b) 6, 0; (c) 1, 5; (d) All of the above, there is no single minimum.
## Note that all pairs add to 6
themin= min(rss)
betas[which(rss==themin),]
# Answer: All of the above, there is no single minimum.

# It's fairly clear from just looking at the numbers, but we can also visualize 
# the sum of squared residuals over our grid with the imagemat() function from rafalib:
library(rafalib)
## plot the pairs what are minimum
themin=min(rss)
plot(betas[which(rss==themin),])
# There is clearly not a single beta which optimizes the least squares equation, 
# due to collinearity, but an infinite line of solutions which produce an identical 
# sum of squares values.

## QR Exercises

# We will use the spider dataset to try out the QR decomposition as a solution to linear models. 
# Load the full spider dataset, by using the code in the Interactions and Contrasts: 
# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html
# book page. Run a linear model of the friction coefficient with two variable (no interactions):

fit <- lm(friction ~ type + leg, data=spider)
# The solution we are interested in solving is:
betahat <- coef(fit)
# So for our matrix work,
Y <- matrix(spider$friction, ncol=1)
X <- model.matrix(~ type + leg, data=spider)
# In the material on QR decomposition:
# http://genomicsclass.github.io/book/pages/qr_and_regression.html
# we saw that the solution for beta is:
# R Beta = Q^T Y

# QR Exercises #1
# What is the first row, first column element in the Q matrix for this linear model?
Q = qr.Q(qr(X))
Q[1,1]
# Answer: -0.05954913

# QR Exercises #2
# What is the first row, first column element in the R matrix for this linear model?
R = qr.R(qr(X))
R[1,1]
# Answer: -16.79286

# QR Exercises #3
# What is the first row, first column element of Q^T Y?
# Use crossprod() as in the book page
crossprod(Q, Y)[1]
# Answer: -13.79872520

# Finally convince yourself that the QR gives the least squares solution by putting 
# all the pieces together:
# R^-1 (Q^T Y) compared to Betahat


### Going further
# It is important to keep in mind that linear models can be extended in many directions. 
# We have produced a page in the book which gives some description and examples of extensions
# http://genomicsclass.github.io/book/pages/linear_models_going_further.html
# to linear models, which you might come across in analyzing data in the life sciences. 
# Some of these topics, in particular GLM and many simultaneous linear models will be covered 
# in the other courses of the series.
# https://learning.edx.org/course/course-v1:HarvardX+PH525.2x+3T-2015/home
# Data Analysis for Life Sciences 2: Introduction to Linear Models and Matrix Algebra

# Robust linear models
# Generalized linear models (GLM)
# Mixed effects linear models
# Bayesian linear models
# Penalized linear models
# Many simultaneous linear models


## Quiz
# Load the spider dataset like this:
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
# The t-value for the contrast of leg pair L4 vs leg pair L2 is constructed by taking the difference 
# of the coefficients legL4 and legL2, and then dividing by the standard error of the difference. 
# In the last question we will explore how the standard error of the difference is calculated here.
# In the book page for contrasts (see the heading, Contrasting the coefficients), we saw that 
# in general, for a contrast vector C, the standard error of the contrast (C Betahat) is:
# SQRT(C Sigma C^T)

# Sigma, is the covariance matrix of beta-hat (Betaht. The covariance matrix contains elements 
# which give the variance or covariance of elements in beta-hat. The elements on the diagonal 
# of the Sigma matrix give the variance of each element in beta-hat. The square root of these 
# is the standard error of the elements in beta-hat. The off-diagonal elements of Sigma give 
# the covariance of two different elements of the beta-hat matrix. So Sigma[1,2] gives the 
# covariance of the first and second element of beta-hat. The Sigma matrix is symmetric, 
# which means Sigma[i,j] = Sigma[j,i].

# But we can also work out in this simple case, where we simply subtract one coefficient 
# from another, using the formula for the variance of sums of random variables:
# Var(Betahat_L4 - Betahat_L2) = Var(Betahat_L4) + Var(Betahat_L2) - 2Cov(Betahat_L4, Betahat_L2)

# In the book page, we computed Sigma using:
fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)
X <- model.matrix(~ type + leg, data=spider)
(Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))
# Our contrast matrix is:
C <- matrix(c(0,0,-1,0,1),1,5)

# Question #1
# Using Sigma, what is Cov(Betahat_L4, Betahat_L2)?
Sigma[3,5]
# Answer: 0.0006389179

# On your own, you should confirm that
# sqrt(Var(beta-hat_L4 - beta-hat_L2)) = sqrt(Var(beta-hat_L4) + Var(beta-hat_L2) - 2 Cov(beta-hat_L4, beta-hat_L2))
# is equal to
sqrt(C %*% Sigma %*% t(C))
# is equal to the standard error from the contrast() for the leg L4 vs L2 difference.

# In the video we briefly mentioned the analysis of variance (or ANOVA, performed in R using 
# the anova() function), which allows us to test whether a number of coefficients are 
# equal to zero, by comparing a linear model including these terms to a linear model where 
# these terms are set to 0.

# The book page for this section 
# http://genomicsclass.github.io/book/pages/interactions_and_contrasts.html
# has a section, "Testing all differences of differences", which explains the ANOVA concept 
# and the F-test in some more detail. You can read over that section before or after 
# the following question.

# In this last question, we will use Monte Carlo techniques to observe the distribution of 
# the ANOVA's "F-value" under the null hypothesis, that there are no differences between groups.

# Suppose we have 4 groups, and 10 samples per group, so 40 samples overall:
N <- 40
p <- 4
group <- factor(rep(1:p,each=N/p))
X <- model.matrix(~ group)
# We will show here how to calculate the "F-value", and then we will use random number to observe 
# the distribution of the F-value under the null hypothesis.

# The F-value is the mean sum of squares explained by the terms of interest (in our case, 
# the group terms) divided by the mean sum of squares of the residuals of a model including 
# the terms of interest. So it is the explanatory power of the terms divided by the 
# leftover variance.

# Intuitively, if this number is large, it means that the group variable explains a lot of 
# the variance in the data, compared to the amount of variance left in the data after using 
# group information. We will calculate these values exactly here:

# First generate some random, null data, where the mean is the same for all groups:

Y <- rnorm(N,mean=42,7)

# The base model we wil compare against is simply Y-hat = mean(Y), which we will call mu0, 
# and the initial sum of squares is the Y values minus mu0:
mu0 <- mean(Y)
initial.ss <- sum((Y - mu0)^2)
# We then need to calculate the fitted values for each group, which is simply the mean 
# of each group, and the residuals from this model, which we will call after.group.ss 
# for the sum of squares after using the group information:
s <- split(Y, group)
after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
# Then the explanatory power of the group variable is the initial sum of squares minus 
# the residual sum of squares:
(group.ss <- initial.ss - after.group.ss)
# We calculate the mean of these values, but we divide by terms which remove the number of 
# fitted parameters. For the group sum of squares, this is number of parameters used to fit 
# the groups (3, because the intercept is in the initial model). 
# For the after group sum of squares, this is the number of samples minus the number of 
# parameters total (So N - 4, including the intercept).
group.ms <- group.ss / (p - 1)
after.group.ms <- after.group.ss / (N - p)
# The F-value is simply the ratio of these mean sum of squares.
f.value <- group.ms / after.group.ms
# What's the point of all these calculations? The point is that, after following these steps, 
# the exact distribution of the F-value has a nice mathematical formula under the null hypothesis. 
# We will see this below.


# Question #2
# Set the seed to 1, set.seed(1), then calculate the F-value for 1000 random versions of Y. 
# What is the mean of these F-values?

set.seed(1)
Fs = replicate(1000, {
  Y = rnorm(N,mean=42,7)
  mu0 = mean(Y)
  initial.ss = sum((Y - mu0)^2)
  s = split(Y, group)
  after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2)))
  (group.ss = initial.ss - after.group.ss)
  group.ms = group.ss / (p - 1)
  after.group.ms = after.group.ss / (N - p)
  f.value = group.ms / after.group.ms
  return(f.value)
})
mean(Fs)
# Answer: 1.069771

# On your own, you may wish to plot the distribution of the 1000 F-values:
hist(Fs, col="grey", border="white", breaks=50, freq=FALSE)
# Overlay the theoretical F-distribution, with parameters df1=p - 1, df2=N - p.
xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")
# This is the distribution which is used to calculate the p-values for the ANOVA table 
# produced by anova(). 

