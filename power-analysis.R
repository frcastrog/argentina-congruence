#---------------------------------Power Analysis-------------------------------#
#-Author: Francisca Castro ------------------------ Created: February 28, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 03, 2024-#

pacman::p_load(pwr, InteractionPoweR)

options(scipen=999)

set.seed (12345)

### Sample size 
# Assuming a 5% lead is significant
effect_size <- 0.05  # 5% difference

# Calculate Cohen's h for the effect size
h <- ES.h(p1 = 0.5, p2 = 0.5 + effect_size)

# Conduct the power analysis
power_result <- pwr.2p.test(h = h, sig.level = 0.05, power = 0.80)

# The function gives the sample size for each group
# For two groups (two candidates), you would need twice this amount
total_sample_size <- power_result$n * 2

# Output the total sample size
total_sample_size


# FULL SAMPLE POWER ANALYSIS

full_sample <- power_interaction_r2(
  alpha = 0.05,             # alpha, for the power analysis
  N = 1800,                 # sample size
  r.x1x2.y = .15,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .2,              # correlation between x1 and y
  r.x2.y = .1,              # correlation between x2 and y
  r.x1.x2 = .2              # correlation between x1 and x2
)

full_sample

# INTERACTION POWER ANALYSIS

power_h2_y1y2 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 800,                  # sample size (total sample size)
  r.x1x2.y = 0.100,         # interaction effect to test: correlation between x1*x2 and y (evaluation)
  r.x1.y = 0.100,           # correlation between x1 and y (evaluation)
  r.x2.y = 0.000,           # correlation between x2 and y (evaluation)  
  r.x1.x2 = 0.000,          # correlation between x1 and x2 
  k.y =  2,                 # categories Y1
  k.x1 = 2                  # categories treatment group
)

power_h2_y1y2

power_h2_y3 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 800,                  # sample size (total sample size)
  r.x1x2.y = 0.100,         # interaction effect to test: correlation between x1*x2 and y (evaluation)
  r.x1.y = 0.100,           # correlation between x1 and y (evaluation)
  r.x2.y = 0.000,           # correlation between x2 and y (evaluation)  
  r.x1.x2 = 0.000,          # correlation between x1 and x2 
  k.y =  10,                # categories Y1
  k.x1 = 2                  # categories treatment group
)

power_h2_y3

power_h3_y1y2 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 800,                  # sample size (total sample size)
  r.x1x2.y = 0.100,         # interaction effect to test: correlation between x1*x2 and y (evaluation)
  r.x1.y = 0.100,           # correlation between x1 and y (evaluation)
  r.x2.y = -0.100,          # correlation between x2 and y (evaluation)  
  r.x1.x2 = 0.000,          # correlation between x1 and x2 
  k.y =  2,                 # categories Y1
  k.x1 = 2                  # categories treatment group
)

power_h3_y1y2

power_h3_y3 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 800,                  # sample size (total sample size)
  r.x1x2.y = 0.100,         # interaction effect to test: correlation between x1*x2 and y (evaluation)
  r.x1.y = 0.100,           # correlation between x1 and y (evaluation)
  r.x2.y = -0.100,          # correlation between x2 and y (evaluation)  
  r.x1.x2 = 0.000,          # correlation between x1 and x2 
  k.y =  10,                # categories Y1
  k.x1 = 2                  # categories treatment group
)

power_h3_y3
