#' ---
#' title: "Analysis of the data from the annealing softmax algorithm"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' last update: Wed Apr 04 11:05:10 2018
#' 

library(tidyverse)
library(feather)

theme_set(theme_minimal())

#' *Case when the means of the arms are c(0.1, 0.1, 0.1, 0.1, 0.9)*

data <- read_feather("data/2018-04-04_annealing-softmax-results.feather")
glimpse(data)

#' Average reward as a function of time

avg_reward <- data %>% group_by(time) %>% 
                       summarize(mean_reward = mean(reward))

dev.new()
ggplot(avg_reward) +
  geom_line(aes(x = time, y = mean_reward)) +
  labs(x = "Time",
       y = "Average reward",
       title = "Performance of the Annealing Softmax algorithm")

#' Probability of selecting the best arm over time

freq_correct <- data %>% group_by(time) %>% 
                         summarize_at(vars(chosen_arm), function(x) mean(x == 5))

ggplot(freq_correct) +
  geom_line(aes(x = time, y = chosen_arm)) +
  labs(x = "Time",
       y = "Probability of choosing the correct arm",
       title = "Performance of the Annealing Softmax algorithm")


#' *Case when the means of the arms are c(0.12, 0.1, 0.1, 0.1, 0.1)*

data <- read_feather("data/2018-04-04_annealing-softmax-results-nodiff.feather")
glimpse(data)

#' Average reward as a function of time

avg_reward <- data %>% group_by(time) %>% 
                       summarize(mean_reward = mean(reward))

ggplot(avg_reward) +
  geom_line(aes(x = time, y = mean_reward)) +
  labs(x = "Time",
       y = "Average reward",
       title = "Performance of the Annealing Softmax algorithm")

#' Probability of selecting the best arm over time

freq_correct <- data %>% group_by(time) %>% 
                         summarize_at(vars(chosen_arm), function(x) mean(x == 1))

ggplot(freq_correct) +
  geom_line(aes(x = time, y = chosen_arm)) +
  labs(x = "Time",
       y = "Probability of choosing the correct arm",
       title = "Performance of the Annealing Softmax algorithm")

#' Very interesting results can be observed from the above plots!