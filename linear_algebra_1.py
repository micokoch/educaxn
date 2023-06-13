# Intro to linear models and matrix algebra
import numpy as np
import matplotlib.pyplot as plt

# Play with values of μ and σ to find the best fit.
μ = 160 ; σ = 6
p = [μ, σ]
histogram(p)

x = np.random.normal(μ, σ, 250)
plt.hist(x)
plt.show() 
plt.clf()
