library(tidyverse)
library(HistData)
data("GaltonFamilies")

# Section 1: Introduction to Regression

# 1.3 Stratification and Variance Explained
# ==========================================

# Video 1.3-1 Anscombe's Quartet/Stratification

# Key points

# - Correlation is not always a good summary of the relationship between two variables.
# - The general idea of conditional expectation is that we stratify a population into groups and compute summaries in each group.
# - A practical way to improve the estimates of the conditional expectations is to define strata of with similar values of x.
# - If there is perfect correlation, the regression line predicts an increase that is the same number of SDs for both variables. If there 
# is 0 correlation, then we don’t use x at all for the prediction and simply predict the average  𝜇𝑦 . For values between 0 and 1, 
# the prediction is somewhere in between. If the correlation is negative, we predict a reduction instead of an increase.

# Code

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# Video 1.3-2 Bivariate Normal Distribution

# Key points

# - When a pair of random variables are approximated by the bivariate normal distribution, scatterplots look like ovals. They can be thin (high correlation) or circle-shaped (no correlation).
# - When two variables follow a bivariate normal distribution, computing the regression line is equivalent to computing conditional expectations.
# - We can obtain a much more stable estimate of the conditional expectation by finding the regression line and using it to make predictions.

# Code

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)


# Video 1.3-3

# Video 1.3-4 
