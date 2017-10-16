# This is parametric bootstrapping to estimate standard errors for the
# parameters of the estimated Gamma distribution in part B.2 of Problem 5
# of Assignment 2-1 of ELEC 321 - Part 1.

# Script parameters:
NUM_OF_SAMPLES <- 1000

SAMPLE_SIZE <- 15
# Method of Moment parameter estimates from part B.2:
MoM_ALPHA <- 3243.6
MoM_LAMBDA <- 271.2
  
# Step 1: Generate 1000 samples of size 15 from the 
#         estimated distribution.
# Step 2: For each sample calculate bootstrap estimates 
#         of the estimated distribution parameters.
# Step 3: Estimate the standard errors for the parameters
#         of the estimated distribution from the bootstrap
#         values of the parameters.

# Step 1&2:
alpha_bs <- NULL # Alpha Bootstrap Estimates
lambda_bs <- NULL # Lambda Bootstrap Estimates
for (i_ in 1:NUM_OF_SAMPLES) {
  sample_ <- rgamma(SAMPLE_SIZE, shape=MoM_ALPHA, rate=MoM_LAMBDA)
  # Calculate sample_ parameters estimates assuming a gamma ditribution
  mu_estimate <- mean(sample_)
  var_estimate <- var(sample_)
  alpha_estimate <- (mu_estimate ** 2) / var_estimate
  lambda_estimate <- mu_estimate / var_estimate
  
  alpha_bs <- c(alpha_bs, alpha_estimate)
  lambda_bs <- c(lambda_bs, lambda_estimate)
}

# Step 3:
print("Standard Deviation Estimate of Bootstrap Alphas:")
print(sd(alpha_bs))
print("Standard Deviation Estimate of Bootstrap Lambdas:")
print(sd(lambda_bs))


