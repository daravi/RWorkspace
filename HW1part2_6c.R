# ELEC 321 Part 2 Assignment 1 Question 3 Part D
NUM_OF_SAMPLES <- 10**4

get_compression_rate <- function(prob_0) {
  # generate a number from 0 to 1 with uniform probability
  pvec <- runif(NUM_OF_SAMPLES, min = 0, max = 1)
  # if pval between 0 and p we choose X = 0, else X = 1
  xvec <- NULL
  for(idx in 1:NUM_OF_SAMPLES) {
    pval <- pvec[idx]
    if (pval <= prob_0) {
      x <- 0
    } else {
      x <- 1
    }
    xvec <- c(xvec, x)
  }
  
  zz <- gzfile("pt2-hw1-q6.gz", "w")
  cat(xvec , file = zz, sep = "")
  close(zz)
  Sys.sleep(0.01)
  compressed_size <- file.size('pt2-hw1-q6.gz')
  bits_per_symbol <- compressed_size * 8 / NUM_OF_SAMPLES
  return(bits_per_symbol)
}

for (idx in 1:20) {
  # probabilities for 0 are 0.05, 0.10, ..., 1.0
  prob_0 <- idx / 20.0
  cr_total <- 0
  # run the experiment 20 times for each p value
  for (idx in 1:20) {
    cr_total <- cr_total + get_compression_rate(prob_0)
  }
  print(cr_total/20.0)
}

file.remove("pt2-hw1-q6.gz")
