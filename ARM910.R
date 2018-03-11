#' ---
#' title: "Chapter 9 and 10 - Gelman and Hill"
#' author: Pavan Gurazada
#' output: github_document
#' ---
#' 
#' last update: Sun Mar 11 04:03:36 2018

library(arm)
library(caret)
library(tidyverse)
library(foreign)

#' Exercise 9.4
#' This experiment is conducted on 2400 persons. The data is as follows:

exp_data <- data.frame(category = c(rep(1, 300), rep(2, 300), rep(3, 500), 
                                    rep(4, 500), rep(5, 200), rep(6, 200),
                                    rep(7, 200), rep(8, 200)),
                       x = c(rep(0, 300), rep(1, 300), rep(0, 500), rep(1, 500), 
                             rep(0, 200), rep(1, 200), rep(0, 200), rep(1, 200)),
                       T = c(rep(0, 300), rep(0, 300), rep(1, 500), rep(1, 500), 
                             rep(0, 200), rep(0, 200), rep(1, 200), rep(1, 200)),
                       y0 = c(rep(4, 300), rep(4, 300), rep(4, 500), rep(4, 500), 
                             rep(10, 200), rep(10, 200), rep(10, 200), rep(10, 200)),
                       y1 = c(rep(6, 300), rep(6, 300), rep(6, 500), rep(6, 500), 
                             rep(12, 200), rep(12, 200), rep(12, 200), rep(12, 200)))

exp_data$y <- ifelse(exp_data$T == 0, exp_data$y0, exp_data$y1)

glimpse(exp_data)

summary(lm(y ~ T + x, data = exp_data))

exp_data %>% group_by(T = as.factor(T)) %>% 
             summarize(n = n(),
                       avg_y = mean(y))
             
#' Exercise 9.10, 9.11
#' Sesame street

sesame_data <- read.dta("data/sesame.dta")
glimpse(sesame_data)

summary(lm(postlet ~ encour + prelet, sesame_data))
summary(lm(postlet ~ as.factor(viewcat) + prelet, sesame_data))
summary(lm(postnumb ~ encour + prenumb, sesame_data))
summary(lm(postnumb ~ as.factor(viewcat) + prenumb, sesame_data))

