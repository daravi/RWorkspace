NUM_OF_EXPS = 100000

find_prob_dead_detected <- function(num_of_experiments) {
  total_num_of_dead_detected <- 0
  for (count in 1:num_of_experiments) {
    sensA_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.8,0.2))
    sensB_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.9,0.1))
    if (sensA_detects_dead | sensB_detects_dead) {
      total_num_of_dead_detected <- total_num_of_dead_detected + 1
    }
  }
  prob_dead_detected <- total_num_of_dead_detected / 
                        num_of_experiments
  return(prob_dead_detected)
}

find_prob_dead_detected_by_1_only <- function(num_of_experiments) {
  total_num_detected_by_1_only <- 0
  for (count in 1:num_of_experiments) {
    sensA_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.8,0.2))
    sensB_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.9,0.1))
    if (xor(sensA_detects_dead, sensB_detects_dead)) {
      total_num_detected_by_1_only <- total_num_detected_by_1_only + 1
    }
  }
  prob_dead_detected_by_1_only <- total_num_detected_by_1_only / 
                                    num_of_experiments
  return(prob_dead_detected_by_1_only)
}

find_prob_locating_dead <- function(num_of_experiments) {
  total_num_of_dead_located <- 0
  for (count in 1:num_of_experiments) {
    sensA_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.8,0.2))
    sensB_detects_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.9,0.1))
    
    if (sensA_detects_dead & sensB_detects_dead) {
      total_num_of_dead_located <- total_num_of_dead_located + 1
    } else {
      if (sensA_detects_dead) {
        sensA_locates_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.7,0.3))
      } else {
        sensA_locates_dead <- FALSE
      }

      if (sensA_detects_dead) {
        sensB_locates_dead <- sample(c(TRUE,FALSE), 1, prob=c(0.4,0.6))
      } else {
        sensB_locates_dead <- FALSE
      }

      if (sensA_locates_dead | sensB_locates_dead) {
        total_num_of_dead_located <- total_num_of_dead_located + 1
      }
    }
    
  }
  prob_locating_dead <- total_num_of_dead_located / 
                        num_of_experiments
  return(prob_locating_dead)
}

print(find_prob_dead_detected(num_of_experiments=NUM_OF_EXPS))
print(find_prob_dead_detected_by_1_only(num_of_experiments=NUM_OF_EXPS))
print(find_prob_locating_dead(num_of_experiments=NUM_OF_EXPS))

