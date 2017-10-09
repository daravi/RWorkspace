EXP_SIZE = 100000
P_D = 0.15

given_defective <- c(0.80,0.64,0.86,0.80,0.74,0.76,0.77,0.84,0.85,0.75)
given_non_defective <- c(0.20,0.16,0.07,0.09,0.18,0.13,0.17,0.14,0.21,0.08)
test_probs <- list(d=given_defective, nd=given_non_defective)

run_tests <- function(is_defective) {
  if (is_defective) {
    T01_result <- sample(c(TRUE,FALSE), 1, prob=c(test_probs[["d"]][01],1-test_probs[["d"]][01]))
    T02_result <- sample(c(TRUE,FALSE), 1, prob=c(test_probs[["d"]][02],1-test_probs[["d"]][02]))
  } else {
    T01_result <- sample(c(TRUE,FALSE), 1, prob=c(test_probs[["nd"]][01],1-test_probs[["nd"]][01]))
    T02_result <- sample(c(TRUE,FALSE), 1, prob=c(test_probs[["nd"]][02],1-test_probs[["nd"]][02]))
  }
  return(c(T01_result,T02_result))
}

I2 <- run_tests(TRUE)

num_of_I2 <- 0
num_of_D_I2 <- 0

counter <- 0

while (num_of_I2 < EXP_SIZE)
{
  is_defective <- sample(c(TRUE,FALSE), 1, prob=c(P_D,1-P_D))
  test_results <- run_tests(is_defective)
  
  counter <- counter + 1

  if (identical(test_results, I2)) {
    num_of_I2 <- num_of_I2 + 1
    if (is_defective) {
      num_of_D_I2 <- num_of_D_I2 + 1
    }
  }
}
print(counter)
print(num_of_D_I2)
print(num_of_I2)
pD_I2 <- num_of_D_I2 / num_of_I2

print(pD_I2)