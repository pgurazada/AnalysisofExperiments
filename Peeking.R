#' ---
#' title: "What if I peek early at my experiment results?"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Thu Mar 29 06:37:41 2018
#' 

library(tidyverse)
library(ggthemes)

theme_set(theme_few())

#' The setting is as follows: A website wants to check if the change in the
#' color of their sign-up button will increase the click through rate. The
#' current color of the button is red and the current click-through-rate is 2%.
#' This means that the probability that a randomly chosen user will click on the
#' button is 2%. Without loss of generality, let us assume that the click
#' probability among a set of 10,000 customers is drawn randomly from a normal
#' distribution of mean 0.02 and standard deviation 0.005. We can always change
#' these numbers to reflect what is known as a business reality.
#'
#' Let us begin with an A/A test. This is a situation where all customers are
#' exposed to the same signal but the answer is known in advance. This is useful
#' to understand the inherent variance in the experimentation

n_customers <- 1e4

click_prob <- rnorm(n_customers, mean = 0.02, sd = 0.005)

initial_status <- rep(0, n_customers) # Initialize all consumers to "did not click"

cond <- sample(c("Treatment", "Control"), 
               size = n_customers, 
               replace = TRUE,
               prob = c(0.5, 0.5)) # assign the customers randomly to treatment and control

current_status <- ifelse(runif(n_customers) < click_prob, 1, 0)

click_data_AA <- data.frame(cond, initial_status, current_status)

#' If we analyze the data after all customers have participated

click_data_AA %>% group_by(cond) %>% 
                  summarize_at(vars(initial_status, current_status),
                               funs(mean, sd), na.rm = TRUE)

#' If we peek early since the opportunity cost of running a bad experiment is
#' high, i.e.,  we stop after the first 1000 customers

click_data_AA %>% sample_n(1000) %>% 
                  group_by(cond) %>% 
                  summarize(n_samples = n(),
                            avg_click = mean(current_status),
                            sd_click = sd(current_status))

summary(lm(current_status ~ cond, data = click_data_AA))

#' The above results indicate that we cannot reject the null hypothesis that
#' there is no difference between treatment and control
#'
#' Now let us assume that there is really a difference in the click-through
#' rates for Treatment and Control.
#'
#' Let us assume customers shown a blue button are twice more likely to click
#' than the current red one

initial_status <- rep(0, n_customers) # Initialize all consumers to "did not click"

cond <- sample(c("Treatment", "Control"), 
               size = n_customers, 
               replace = TRUE,
               prob = c(0.5, 0.5))

click_prob <- ifelse(cond == "Treatment", 
                     rnorm(1, 0.04, 0.005), 
                     rnorm(1, 0.02, 0.005)) 

current_status <- ifelse(runif(n_customers) < click_prob, 1, 0)

click_data_AB <- data.frame(cond, initial_status, current_status)

#' Now, lets analyze the full set of observations

click_data_AB %>% group_by(cond) %>% 
                  summarize(n_samples = n(),
                            avg_click = mean(current_status),
                            sd_click = sd(current_status))

#' Now if we had stopped when we reached 1000 customers
#' We can run the below snippet multiple times and notice that the
#' variability in average clicks earned varies tremendously

click_summary_AB <- click_data_AB %>% sample_n(1000) %>% 
                                      group_by(cond) %>% 
                                      summarize(n_samples = n(),
                                                avg_click = mean(current_status),
                                                sd_click = sd(current_status))

ggplot(click_summary_AB, aes(x = cond, y = avg_click)) +
  geom_pointrange(aes(ymin = avg_click - sd_click, ymax = avg_click + sd_click),
                  size = 0.8) +
  geom_pointrange(aes(ymin = avg_click - 2 * sd_click, 
                      ymax = avg_click + 2 * sd_click),
                  size = 0.3) +
  geom_hline(aes(yintercept = 0.02), linetype = "dashed") +
  scale_x_discrete("Condition", 
                   labels = c("Control", "Treatment")) +
  scale_y_continuous("", breaks = c(c(0.02, 0.1, 0.2, 0.3))) +
  coord_flip() +
  ggtitle("Average click through percentage by condition", 
          subtitle = "(random sample of 1000 customers)")
