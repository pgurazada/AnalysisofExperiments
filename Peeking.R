#' ---
#' title: "What if I peek early at my experiment results?"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Wed Mar 28 14:11:08 2018
#' 

library(tidyverse)

theme_set(theme_bw())

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

summary(lm(current_status ~ cond, data = click_data))

#' The above results indicate that we cannot reject the null hypothesis that
#' there is no difference between treatment and control
#'
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

click_data_AB %>% sample_n(1000) %>% 
                  group_by(cond) %>% 
                  summarize(n_samples = n(),
                            avg_click = mean(current_status),
                            sd_click = sd(current_status))

#' We can run the above snippet multiple times and notice that the
#' variability in average clicks earned varies tremendously

dev.new()
ggplot(click_data_AB) + 
  geom_histogram(aes(x = current_status, fill = cond), bins = 2) +
  labs(x = "Current status",
       y = "Count",
       fill = "Condition",
       title = "Results of experiment",
       subtitle = "(Full sample - 10,000 observations)")

ggplot(click_data_AB %>% sample_n(1000)) + 
  geom_histogram(aes(x = current_status, fill = cond), bins = 2) +
  labs(x = "Current status",
       y = "Count",
       fill = "Condition",
       title = "Results of experiment",
       subtitle = "(Random sample - 1000 observations)")

