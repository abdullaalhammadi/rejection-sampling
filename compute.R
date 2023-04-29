# Load necessary libraries
library(tidyverse)
library(reshape2)

# Set seed for reproducibility
set.seed(464)

# Initialize loop counter and storage for 10,000 values
i <- 1
vals <- numeric(10000)

# Initialize storage for 1,000 samples from a mixture of distributions
norms <- numeric(1000)

# Sample 1,000 values from a mixture of uniform, chi-squared, and Poisson distributions, set to resemble an unknown distribution.
for(i in 1:1000) {
  norms[i] <- sample(c(runif(1, -2, 4), rchisq(1, 2), rpois(1, 3)), 1)
}

# Plot a histogram of the sampled mixture values with density (probability) on the y-axis
hist(norms, freq = FALSE)

# Estimate the density of the mixture values
dens <- density(norms)
pdf <- approxfun(dens)

# Initialize loop counter and storage for another 10,000 values
i <- 1
vals <- numeric(10000)

# Generate 10,000 samples from the estimated mixture density using rejection sampling
while(i < 10000) {
  u <- runif(1)
  x <- rnorm(1, mean = 2, sd = sqrt(4))
  ratio <- pdf(x)/dnorm(x, mean = 2, sd = sqrt(4))
  if(!is.na(ratio) && u < ratio) {
    vals[i] <- x
    i <- i+1
  }
}

# Plot the estimated density of the sampled mixture values
plot(density(norms))

# Plot a histogram of the 10,000 values sampled from the estimated mixture density
hist(vals)

# Perform a Kolmogorov-Smirnov test comparing the sampled values to the original mixture values
ks.test(vals, norms)

# Combine the exponential and mixture values into a single data frame and reshape into long format
data <- data.frame(vals, norms) %>% melt()

# Create a side-by-side histogram of the exponential and mixture values using ggplot2
data %>% ggplot(aes(x = value, fill = variable)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 10)
