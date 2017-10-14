# # Part (b)
# 1 - pbinom(0, 2, 0.1)
# 1 - pbinom(1, 2, 0.1)
# 1 - pbinom(4, 11, 0.1)
# 1 - pbinom(6, 11, 0.1)

# Part (c)
ppois(q)
EXP_SIZE <- 100000

run_exp <- function() {
  num_of_successes <- 0
  num_of_failures <- 0
  while (num_of_successes < 20) {
    success <- sample(c(TRUE,FALSE), 1, prob=c(0.1,0.9))
    if (success)
      num_of_successes <- num_of_successes + 1
    else
      num_of_failures <- num_of_failures + 1
  }
  return(num_of_failures)
}

total_failures <- 0
for (i in 1:EXP_SIZE) {
  num_of_failures <- run_exp()
  total_failures <- total_failures + num_of_failures
}
mu <- total_failures * 1.0 / EXP_SIZE

sum_var <- 0
for (i in 1:EXP_SIZE) {
  num_of_failures <- run_exp()
  sum_var <- sum_var + ((num_of_failures - mu) ** 2)
}
var <- sum_var * 1.0 / EXP_SIZE
sd <- sqrt(var)

print("MU:")
print(mu)
print("SD:")
print(sd)
