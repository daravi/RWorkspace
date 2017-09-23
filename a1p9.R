num_of_experiments <- 1000

play_a_round <- function(PaulsMoney, LindasMoney) {
  number_of_games <- 0
  while (PaulsMoney > 0 && LindasMoney > 0) {
    LindaWinsGame <- sample(c(TRUE,FALSE), size = 1)
    if (LindaWinsGame) {
      PaulsMoney <- PaulsMoney - 2
      LindasMoney <- LindasMoney + 2
    } else {
      PaulsMoney <- PaulsMoney + 2
      LindasMoney <- LindasMoney - 2
    }
    number_of_games <- number_of_games + 1;
  }
  return(list(PaulWinsRound = (LindasMoney == 0), number_of_games))
}

run_simulation <- function(PaulsMoney, LindasMoney) {
  PaulWinCount <- 0
  number_of_games_sum <- 0
  for (count in 1:num_of_experiments) {
    results <- play_a_round(PaulsMoney, LindasMoney)
    PaulWinsRound <- results[[1]][1]
    number_of_games <- results[[2]][1]
    if (PaulWinsRound == TRUE) {
      PaulWinCount <- PaulWinCount + 1
    }
    number_of_games_sum <- number_of_games_sum + number_of_games
  }
  
  print('The probability that Paul ends up ruined: ')
  print(1 - PaulWinCount / num_of_experiments)
  print('The expected number of played games: ')
  print(number_of_games_sum * 1.0 / num_of_experiments)
}

run_simulation(50, 50)
run_simulation(50, 500)
run_simulation(50, 5000)
run_simulation(50, 5000000)

# > run_simulation(50, 50)
# [1] "The probability that Paul ends up ruined: "
# [1] 0.505
# [1] "The expected number of played games: "
# [1] 617.05
# 
# > run_simulation(500, 50)
# [1] "The probability that Paul ends up ruined: "
# [1] 0.911
# [1] "The expected number of played games: "
# [1] 6143.433
# 
# > run_simulation(5000, 50)
# [1] "The probability that Paul ends up ruined: "
# [1] 0.991
# [1] "The expected number of played games: "
# [1] 53253.5
# 
# > run_simulation(5000000, 50)
# [1] "The probability that Paul ends up ruined: "
# [1] 1
# [1] "The expected number of played games: "
# [1] 821708
