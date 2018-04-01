library(tidyverse)

theme_set(theme_minimal())

data <- read_csv("data/2018-04-01_epsilon-greedy.csv", progress = TRUE)
glimpse(data)

#' average reward as a function of time

avg_reward <- data %>% group_by(time, epsilon) %>% 
                       summarize_at(vars(reward), funs(mean))

dev.new()

ggplot(avg_reward) +
  geom_line(aes(x = time, y = reward, 
                group = epsilon, color = epsilon), size = 1) +
  scale_color_gradient(low = "grey", high = "black") +
  labs(x = "Time",
       y = "Average reward",
       title = "Performance of the Epsilon-Greedy Algorithm",
       color = "epsilon\n")

#' Frequency of selecting the correct arm

freq_correct <- data %>% group_by(time, epsilon) %>% 
                         summarize_at(vars(chosen_arm), function(x) mean(x == 5))

ggplot(freq_correct) + 
  geom_line(aes(x = time, y = chosen_arm, 
                group = epsilon, color = epsilon), size = 1) +
  scale_color_gradient(low = "grey", high = "black") +
  labs(x = "Time",
       y = "Probability of choosing the correct arm",
       title = "Performance of the Epsilon-Greedy Algorithm",
       color = "epsilon\n")

#' Cumulative reward in each group

avg_cum_reward <- data %>% group_by(time, epsilon) %>% 
                           mutate(cum_sum = cumsum(reward)) %>% 
                           summarize(avg_cumsum = mean(cum_sum))

ggplot(avg_cum_reward) + 
  geom_line(aes(x = time, y = avg_cumsum, 
                group = epsilon, color = epsilon), size = 1) +
  scale_color_gradient(low = "grey", high = "black") +
  labs(x = "Time",
       y = "Average cumulative reward",
       title = "Performance of the Epsilon-Greedy Algorithm",
       color = "epsilon\n")
