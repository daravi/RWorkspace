# ELEC 321 Part 2 Assignment 1 Question 3 Part D
NUM_OF_SAMPLES <- 400

# generate a number from 0 to 1 with uniform probability
pvec <- runif(NUM_OF_SAMPLES, min = 0, max = 1)
# if s between 0 and 0.2 we choose X = -3 and so on...
xvec <- NULL
yvec <- NULL
for(idx in 1:NUM_OF_SAMPLES) {
  pval <- pvec[idx]
  if (pval <= 0.2) {
    x <- -3
  } else if (pval <= 0.5) {
    x <- -1
  } else if (pval <= 0.6) {
    x <- 1
  } else {
    x <- 3
  }
  y <- x**4 + 2*x + 2
  xvec <- c(xvec, x)
  yvec <- c(yvec, y)
}

