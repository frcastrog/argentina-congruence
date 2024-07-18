#---------------------------------Power Analysis-------------------------------#
#-Author: Francisca Castro ------------------------ Created: February 28, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: July 18, 2024-#

pacman::p_load(pwr, InteractionPoweR, ggplot2)

options(scipen=999)

set.seed (123456)

####################### Power analysis for binary outcomes ####################

# Parameters for logistic regression power analysis
odds_ratio <- 1.5
alpha <- 0.05
desired_power <- 0.80
num_groups <- 3  # Two treatment groups and one control group

# Convert odds ratio to Cohen's d (approximation)
log_odds <- log(odds_ratio)
cohen_d <- log_odds / sqrt(3.29)

# Power analysis function for a range of sample sizes
sample_sizes <- seq(30, 1000, by = 10)
power_values <- sapply(sample_sizes, function(n) {
  pwr_result <- pwr.p.test(h = cohen_d, n = n, sig.level = alpha, alternative = "two.sided")
  pwr_result$power
})

# Create a data frame for plotting
power_data <- data.frame(
  SampleSize = sample_sizes,
  Power = power_values
)

# Plot the power curve
ggplot(power_data, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power, linetype = "dashed", color = "red") +
  labs(title = "Power Analysis for Logistic Regression",
       x = "Sample Size per Group",
       y = "Power") +
  theme_minimal()

# Calculate required sample size for each group to achieve desired power
total_sample_size <- pwr.p.test(h = cohen_d, sig.level = alpha, power = desired_power, alternative = "two.sided")$n
sample_size_per_group <- ceiling(total_sample_size / num_groups)
cat("Required sample size per group:", sample_size_per_group, "\n")


####################### Power analysis Continuous outcome ###################### 

# Parameters for linear regression 
effect_size <- 0.15  # Cohen's f2 for a medium effect size
alpha <- 0.05
desired_power <- 0.80
num_predictors <- 5  # Number of predictors in the model

# Function to calculate power for given sample size
calculate_power <- function(n, u, f2, alpha) {
  pwr_result <- pwr.f2.test(u = u, v = n - u - 1, f2 = f2, sig.level = alpha, power = NULL)
  return(pwr_result$power)
}

# Power analysis for a range of sample sizes
sample_sizes <- seq(30, 1000, by = 10)
power_values <- sapply(sample_sizes, calculate_power, u = num_predictors, f2 = effect_size, alpha = alpha)

# Data frame for plotting
power_data <- data.frame(
  SampleSize = sample_sizes,
  Power = power_values
)

# Plot the power curve
ggplot(power_data, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power, linetype = "dashed", color = "red") +
  labs(title = "Power Analysis for Linear Regression",
       x = "Sample Size",
       y = "Power") +
  theme_minimal()

# Calculate required sample size for the desired power
pwr_result_linear <- pwr.f2.test(u = num_predictors, f2 = effect_size, sig.level = alpha, power = desired_power)
sample_size_total <- ceiling(pwr_result_linear$v + num_predictors + 1)
cat("Required total sample size for linear regression:", sample_size_total, "\n")


# Calculate sample size per group assuming equal allocation to 3 groups
num_groups <- 3
sample_size_per_group <- ceiling(sample_size_total / num_groups)
cat("Required sample size per group for linear regression:", sample_size_per_group, "\n")


#- Power analysis for H2 (continuous)

alpha <- 0.05
desired_power <- 0.80
sample_size <- 1600  # Initial sample size guess
correlation_x1_y <- 0.20  # Correlation between predictor x1 and the outcome
correlation_x2_y <- 0.10  # Correlation between predictor x2 and the outcome
correlation_x1_x2 <- 0.09  # Correlation between predictors x1 and x2

# Calculate power for a range of interaction effect sizes
h2_power <- power_interaction_r2(
  N = sample_size,
  r.x1.y = correlation_x1_y,
  r.x2.y = correlation_x2_y,
  r.x1.x2 = correlation_x1_x2,
  r.x1x2.y = seq(0.01, 0.12, 0.005),  # Range of interaction effect sizes
  alpha = alpha
)

# Estimate the minimum detectable effect size for the desired power
h2_estimate <- power_estimate(
  power_data = h2_power,
  x = "r.x1x2.y",
  power_target = desired_power
)

# Estimated MDE
round(h2_estimate, 3)

# Plot the power curve
power_curve_plot <- plot_power_curve(h2_power, x = "r.x1x2.y", power_target = desired_power) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))  # Adjust the margins as needed

ggsave("outputs/power_curve_plot.png", 
       width = 11, height = 7, units = "cm", dpi = 600)
