# Set of all 20 balls in the urn
six_green_balls  <- as.list(rep("g", 6))
six_red_balls    <- as.list(rep("r", 6))
eight_blue_balls <- as.list(rep("b", 8))
urn <- c(six_green_balls, six_red_balls, eight_blue_balls)

find_expd_num_of_red_balls <- function(num_of_experiments) {
  total_num_of_red_balls <- 0
  for (count in 1:num_of_experiments) {
    three_ball_sample <- sample(urn, size=3, replace=FALSE)
    total_num_of_red_balls <- total_num_of_red_balls + 
                              sum(three_ball_sample == "r")
  }
  expd_num_of_red_balls <- total_num_of_red_balls / num_of_experiments
  return(expd_num_of_red_balls)
}

expd_num_of_red_balls <- find_expd_num_of_red_balls(num_of_experiments=100000)

print(expd_num_of_red_balls)