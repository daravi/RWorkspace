EXP_SIZE = 100000

P_A <- 0.005
P_B <- 0.0058823824

get_occurance_time <- function(p) {
  occured <- FALSE
  occ_time <- 0
  while(!occured) {
    occ_time <- occ_time + 1
    occured <- sample(c(TRUE,FALSE), 1, prob=c(p,1-p))
  }
  return(occ_time)
}

B_before_A <- 0
for(i in 1:EXP_SIZE) {
  A1 <- get_occurance_time(P_A)
  B1 <- get_occurance_time(P_B)
  if (B1 < A1) {
    B_before_A <- B_before_A + 1
  }
}

print(B_before_A * 1.0 / EXP_SIZE)