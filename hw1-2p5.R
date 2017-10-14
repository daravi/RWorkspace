# EXPD_PD_I10_TEST_SIZE = 50000
# PD_I10_TEST_SIZE = 100

P_D <- 0.15
P_ND <- 1 - P_D

P_B_D <- c(0.80,0.64,0.86,0.80,0.74,0.76,0.77,0.84,0.85,0.75)
P_B_ND <- c(0.20,0.16,0.07,0.09,0.18,0.13,0.17,0.14,0.21,0.08)
P_B <- list(d=P_B_D, nd=P_B_ND)

# Readable symbols for the first and second dimentions of a matrix
ROWS <- 1
COLS <- 2

run_tests <- function(is_defective) {
  if (is_defective) {
    T01_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][01],1-P_B[["d"]][01]))
    T02_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][02],1-P_B[["d"]][02]))
    T03_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][03],1-P_B[["d"]][03]))
    T04_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][04],1-P_B[["d"]][04]))
    T05_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][05],1-P_B[["d"]][05]))
    T06_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][06],1-P_B[["d"]][06]))
    T07_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][07],1-P_B[["d"]][07]))
    T08_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][08],1-P_B[["d"]][08]))
    T09_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][09],1-P_B[["d"]][09]))
    T10_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["d"]][10],1-P_B[["d"]][10]))
  } else {
    T01_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][01],1-P_B[["nd"]][01]))
    T02_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][02],1-P_B[["nd"]][02]))
    T03_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][03],1-P_B[["nd"]][03]))
    T04_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][04],1-P_B[["nd"]][04]))
    T05_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][05],1-P_B[["nd"]][05]))
    T06_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][06],1-P_B[["nd"]][06]))
    T07_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][07],1-P_B[["nd"]][07]))
    T08_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][08],1-P_B[["nd"]][08]))
    T09_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][09],1-P_B[["nd"]][09]))
    T10_result <- sample(c(TRUE,FALSE), 1, prob=c(P_B[["nd"]][10],1-P_B[["nd"]][10]))
  }
  return(c(T01_result,T02_result,T03_result,T04_result,T05_result,
           T06_result,T07_result,T08_result,T09_result,T10_result))
}

find_pD_I10 <- function(I10, num_of_desired_experiments=0) {
  # To simulate without relying on prbability results:
  # num_of_I10 <- 0
  # num_of_D_I10 <- 0
  # while (num_of_I10 < num_of_desired_experiments)
  # {
  #   is_defective <- sample(c(TRUE,FALSE), 1, prob=c(P_D,1-P_D))
  #   test_results <- run_tests(is_defective)
  #   
  #   if (identical(test_results, I10)) {
  #     num_of_I10 <- num_of_I10 + 1
  #     if (is_defective) {
  #       num_of_D_I10 <- num_of_D_I10 + 1
  #     }
  #   }
  # }
  # pD_I10 <- num_of_D_I10 / num_of_I2
  
  # Calculate using probability results:
  pD_I10 <- P_D
  for (test_num in 1:10) {
    if(I10[test_num]) {
      pTest_D  <- P_B[["d"]][test_num]
      pTest_ND <- P_B[["nd"]][test_num]
    } else {
      pTest_D  <- (1 - P_B[["d"]][test_num])
      pTest_ND <- (1 - P_B[["nd"]][test_num])
    }
    pTest <- pTest_D * P_D + pTest_ND * P_ND
    pD_I10 <- pD_I10 * (pTest_D / pTest)
  }
  return(pD_I10)
}

find_expd_pD_I10 <- function(num_of_experiments) {
  pD_I10_total <- 0
  for (count in 1:num_of_experiments) {
    test_results <- run_tests(TRUE)
    
    pD_I10 <- find_pD_I10(
      test_results,
      PD_I10_TEST_SIZE)
    
    pD_I10_total <- pD_I10_total + pD_I10
  }

  expd_pD_I10 <- pD_I10_total / num_of_experiments
  return(expd_pD_I10)
}

# print(find_expd_pD_I10(num_of_experiments=EXPD_PD_I10_TEST_SIZE))

generate_seqs_I10 <- function(is_defective, N) {
  seqs_I10 <- NULL
  for (i_ in 1:N) {
    seqs_I10 <- c(seqs_I10, run_tests(is_defective))
  }
  dim(seqs_I10) <- c(length(P_B[["d"]]), N)
  
  return(seqs_I10)
}

calculate_seqs_pD_I10 <- function(seqs_I10) {
  seqs_pD_I10 <- NULL
  for (col in 1:dim(seqs_I10)[COLS]) {
    idx <- array(c(1:dim(seqs_I10)[ROWS],
                   seq(col,col,length.out=dim(seqs_I10)[ROWS])), 
                 dim=c(dim(seqs_I10)[ROWS],2))
    I10 <- seqs_I10[idx]
    print(I10)
    pD_I10 <- find_pD_I10(I10)
    print(pD_I10)
    seqs_pD_I10 <- c(seqs_pD_I10, pD_I10)
  }
  
  return(seqs_pD_I10)
}

calculate_f <- function(is_defective, seqs_pD_I10, cutoff) {
  print("is defective?")
  print(is_defective)
  num_of_errors <- 0
  for (pD_I10 in seqs_pD_I10) {
    if (pD_I10 > cutoff) { # will not ship
      if (!is_defective) { # not defective
        num_of_errors <- num_of_errors + 1
      }
    } else {               # will ship
      if (is_defective) {  # is defective
        num_of_errors <- num_of_errors + 1
      }
    }
  }
  print("num of errors:")
  print(num_of_errors)
  f <- num_of_errors / length(seqs_pD_I10)
  return(f)
}

EXP_SIZE = 5
# Part 1
seqs_I10_D <- generate_seqs_I10(is_defective=TRUE, N=EXP_SIZE)
seqs_pD_I10 <- calculate_seqs_pD_I10(seqs_I10_D)

# Part 3
seqs_I10_ND <- generate_seqs_I10(is_defective=FALSE, N=EXP_SIZE)
seqs_pD_I10 <- calculate_seqs_pD_I10(seqs_I10_ND)

# Part 5
# for (a in seq(from=0.01,to=0.99,by=0.2)) {
#   # Part 2 (Estimate shipped given defective)
#   print("a:")
#   print(a)
#   fd <- calculate_f(is_defective=TRUE, seqs_pD_I10, cutoff=a)
#   print("fd:")
#   print(fd)
#   # Part 4 (Estimate not shipped given non-defective)
#   fnd <- calculate_f(is_defective=FALSE, seqs_pD_I10, cutoff=a)
#   print("fnd:")
#   print(fnd)
#   Ja <- fnd*(1-P_D) + fd*P_D
#   print("\n\n")
# }

