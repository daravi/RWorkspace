# ELEC 321 Part 2 Assignment 1 Question 3 Part D
NUM_OF_SAMPLES <- 104
p <- 0.9

# generate a number from 0 to 1 with uniform probability
pvec <- runif(NUM_OF_SAMPLES, min = 0, max = 1)
# if pval between 0 and p we choose X = 0, else X = 1
xvec <- NULL
yvec <- NULL
for(idx in 1:NUM_OF_SAMPLES) {
  pval <- pvec[idx]
  if (pval <= p) {
    x <- 0
  } else {
    x <- 1
  }
  xvec <- c(xvec, x)
}
