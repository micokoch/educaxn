## playing with stats

# R code to plot a pdf for a Gaussian distribution (Bard)

# Define the mean and standard deviation of the Gaussian distribution
mu <- 0
sigma <- 1

# Define the interval of integration
lower <- -5
upper <- 5

# Create a range of values for x
x <- seq(lower, upper, by = 0.1)

# Calculate the pdf for each value of x
pdf <- dnorm(x, mu, sigma)

# Plot the pdf
plot(x, pdf, type = "l", lwd = 2, xlab = "x", ylab = "pdf")

# Calculate the area under the curve
(area <- integrate(dnorm, lower, upper, mu, sigma))
#0.9999994 with absolute error < 8.7e-10

# A different approach
library(pracma)
trapz(x, pdf)
# 0.9999994

