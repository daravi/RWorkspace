# ELEC 321 Part 2 Assignment 1 Question 4
NUM_OF_SAMPLES <- 10000

# generate a number from 0 to 1 with uniform probability
pvecx <- runif(NUM_OF_SAMPLES, min = 0, max = 1)
pvecy <- runif(NUM_OF_SAMPLES, min = 0, max = 1)

# x and y samples are calculated
xvec <- NULL
yvec <- NULL
for(idx in 1:NUM_OF_SAMPLES) {
  pvalx <- pvecx[idx]
  pvaly <- pvecy[idx]
  x <- log((exp(1)-1)*pvalx+1)
  y <- (sqrt(1+8*pvaly)-1) / 2
  xvec <- c(xvec, x)
  yvec <- c(yvec, y)
}

hist(xvec, main = "Histogram for Q4-a")
hist(yvec, main = "Histogram for Q4-b")

curve((exp(x)-1)/(exp(1)-1), from = 0, to = 1, n = 101)
curve((x**2+x)/2, from = 0, to = 1, n = 101)