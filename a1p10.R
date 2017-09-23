num_of_experiments <- 10000

play_a_round <- function() {
  money <- 5000
  gameCount <- 0
  while (gameCount < 200 && money > 0) {
    gameWon <- sample(c(TRUE,FALSE), size = 1)
    if (gameWon) {
      money <- money + 100
    } else {
      money <- money - 100
    }
    gameCount <- gameCount + 1
  }

  return(money)
}

run_simulation <- function() {
  money_sum <- 0
  for (count in 1:num_of_experiments) {
    money <- play_a_round()
    money_sum <- money_sum + money
  }
  
  print('The expected win amount: ')
  print(money_sum * 1.0 / num_of_experiments)
}

run_simulation()

# Example run output:
# [1] "The expected win amount: "
# [1] 4995.74
