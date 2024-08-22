#---------------------------------Power Analysis-------------------------------#
#-Author: Francisca Castro ------------------------ Created: February 28, 2024-#
#-R Version: 4.4.0 ---------------------------------- Revised: August 22, 2024-#

pacman::p_load(pwr, InteractionPoweR, ggplot2)

options(scipen=999)

set.seed (123456)

#################### HI: effect of individual-elite congruence #################

# For this hypothesis, we just focus on the role of congruence, without taking
# into account how elite-mobilized group congruence could affect the respondent's
# evaluation of the candidate. Therefore, only the control group (no treatment)
# is used in this analysis. 

### Binary outcomes: eval_1 and eval_2 ###

# Expected proportions for choosing candidates
p_congruent_chosen <- 0.65  # Probability of choosing the congruent candidate
p_incongruent_chosen <- 0.35  # Probability of choosing the incongruent candidate

# Alpha and desired power
alpha <- 0.05
desired_power <- 0.95

# Effect size (Cohen's h) for proportions
effect_size_h <- ES.h(p_congruent_chosen, p_incongruent_chosen)

# Power analysis (range of sample sizes)
sample_sizes <- seq(30, 1000, by = 10)
power_values <- sapply(sample_sizes, function(n) {
  pwr_result <- pwr.2p.test(h = effect_size_h, n = n, 
                            sig.level = alpha, power = NULL, 
                            alternative = "two.sided")
  pwr_result$power
})

# Data frame for plotting
power_data <- data.frame(
  SampleSize = sample_sizes,
  Power = power_values)

# Plot the power curve
power_plot_h1 <- ggplot(power_data, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power, linetype = "dashed", color = "red") +
  labs(x = "Sample Size per Group",
       y = "Power") +
  theme_minimal()

power_plot_h1

ggsave("outputs/power_plot_h1.png", 
       width = 11, height = 7, units = "cm", dpi = 600)


# Calculate required sample size for each group to achieve desired power
total_sample_size_h1 <- pwr.2p.test(h = effect_size_h, sig.level = alpha, 
                                    power = desired_power, 
                                    alternative = "two.sided")$n

total_sample_size_h1 # 56.59031


### Continuous outcomes: eval_3 ###

# Parameters 
effect_size_d <- 0.70  

# Power analysis for a range of sample sizes
power_values_2 <- sapply(sample_sizes, function(n) {
  pwr_result <- pwr.t.test(d = effect_size_d, n = n, 
                           sig.level = alpha, power = NULL, 
                           type = "two.sample", alternative = "two.sided")
  pwr_result$power})

# Create a data frame for plotting
power_data_2 <- data.frame(
  SampleSize = sample_sizes,
  Power = power_values_2)

# Plot the power curve
power_plot_h1_continuous <- ggplot(power_data_2, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power, linetype = "dashed", color = "red") +
  labs(x = "Sample Size per Group",
       y = "Power") +
  theme_minimal()

power_plot_h1_continuous

ggsave("outputs/power_plot_h1_continuous.png", 
       width = 11, height = 7, units = "cm", dpi = 600)

# Calculate required sample size for each group to achieve desired power
total_sample_size_h1_continuous <- pwr.t.test(d = effect_size_d, sig.level = alpha, 
                                              power = desired_power, 
                                              type = "two.sample", 
                                              alternative = "two.sided")$n

total_sample_size_h1_continuous #54.01938



## H2b/3a: effect of individual-elite congruence and elite-group incongruence ##

### Binary outcomes: eval_1 and eval_2 ###

# Parameters
p1_h2b <- 0.55
p2_h2b <- 0.45
alpha_h2b <- 0.05
desired_power_h2b <- 0.95

# Calculate effect size for H2b
effect_size_h_h2b <- ES.h(p1_h2b, p2_h2b)

# Power analysis (range of sample sizes) for H2b
sample_sizes_h2b <- seq(30, 2000, by = 10)
power_values_h2b <- sapply(sample_sizes_h2b, function(n) {
  pwr_result_h2b <- pwr.2p.test(h = effect_size_h_h2b, n = n, 
                                sig.level = alpha_h2b, power = NULL, 
                                alternative = "two.sided")
  pwr_result_h2b$power})

# Create a data frame for plotting H2b
power_data_h2b <- data.frame(
  SampleSize = sample_sizes_h2b,
  Power = power_values_h2b)

# Plot the power curve for H2b
power_plot_h2b <- ggplot(power_data_h2b, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power_h2b, linetype = "dashed", color = "red") +
  labs(x = "Sample Size per Group",
       y = "Power") +
  theme_minimal()

power_plot_h2b

ggsave("outputs/power_plot_h2b.png", power_plot_h2b, 
       width = 11, height = 7, units = "cm", dpi = 600)

# Calculate required sample size per group for H2b
sample_size_per_group_h2b <- pwr.2p.test(h = effect_size_h_h2b, sig.level = alpha_h2b, 
                                         power = desired_power_h2b, alternative = "two.sided")$n

sample_size_per_group_h2b

# Total sample size (for three groups) for H2b
total_sample_size_h2b <- sample_size_per_group_h2b * 3

total_sample_size_h2b

### Continuous outcomes: eval_3 ###

# Expected mean difference and standard deviation for eval_3
mean_diff_h2b <- 1.5  # diff in means between groups
sd_h2b <- 2.5         # SD of responses

# Calculate Cohen's d for eval_3
effect_size_d_h2b <- mean_diff_h2b / sd_h2b

# Power analysis for a range of sample sizes for eval_3
power_values_eval3_h2b <- sapply(sample_sizes_h2b, function(n) {
  pwr_result_h2b <- pwr.t.test(d = effect_size_d_h2b, n = n, 
                               sig.level = alpha_h2b, power = NULL, 
                               type = "two.sample", alternative = "two.sided")
  pwr_result_h2b$power})

# Create a data frame for plotting eval_3
power_data_eval3_h2b <- data.frame(
  SampleSize = sample_sizes_h2b,
  Power = power_values_eval3_h2b
)

# Plot the power curve for eval_3
power_plot_eval3_h2b <- ggplot(power_data_eval3_h2b, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_hline(yintercept = desired_power_h2b, linetype = "dashed", color = "red") +
  labs(x = "Sample Size per Group",
       y = "Power") +
  theme_minimal()

power_plot_eval3_h2b

# Save and print the plot for eval_3
ggsave("outputs/power_plot_eval3_h2b.png", power_plot_eval3_h2b, 
       width = 11, height = 7, units = "cm", dpi = 600)

# Calculate the required sample size per group for eval_3
sample_size_per_group_h2b_eval3 <- pwr.t.test(d = effect_size_d_h2b, sig.level = alpha_h2b, 
                                              power = desired_power_h2b, 
                                              type = "two.sample", 
                                              alternative = "two.sided")$n


sample_size_per_group_h2b_eval3 #73.16746



