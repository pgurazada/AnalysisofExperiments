#' ---
#' title: "Fast A/B testing "
#' author: Pavan Gurazada
#' date: "February 2018"
#' output: github_document
#' ---

#' `“In God we Trust, all others bring data” - Edwards Deming`
#'
#' Undoubtedly A/B testing is one of those highly researched fields that has
#' also had significant impact on real business. Several online businesses have
#' A/B testing culture rooted deep within the organization. A/B testing is an
#' extension of traditional experimental design. Much of the complexity comes
#' from the scale of such experiments conducted in real-life.
#'
#' The code in this script is based on the presentation
#' [here](https://github.com/robinsones/AB-Testing-Slides/blob/master/AB%20Testing%20in%20the%20Wild.pdf)
#'
#' Like in typical experimental design, the analyst defines the sampling unit,
#' outcome variable precisely (very very important) and checks the independence
#' assumption (i.e., the samples are i.i.d).
#'
#' If we look at an e-commerce website, the sampling unit could be one visit or
#' the entire browsing history from a unique device. If the sampling unit is a
#' single visit then the independence of samples is not guaranteed (one can
#' browse in one visit and buy only in the next). If the sampling unit is the
#' history from a unique device then the observations are heavy with noise.
#'
#' At its simplest, A/B testing is simply allocating users to one of two
#' scenarios randomly and measuring if there is any difference in a measurable,
#' intended outcome. 
#'
#' Always check the distribution of the outcome variable for skewness.
#'
#' Pause, and wait till the full experiment has run. Do not peek early, and if
#' you do, don't conclude. Decisions based on poorly run experminents are
#' usually devastating. Use historical data for some metrics on how long experiments
#' take to stabilize.
#' 
#' In this script we create a large experiment and run a simple analysis on it.

library(tidyverse)

set.seed(20130810)

#' *Generate Data to play with*
#'
#' Fix the number of browsers and the number of bootstrap samples to draw from
#' these. Draw up samples for browser ids and visits from these ids.
#' 

nbrowsers <- 8e5
nsamples <- 1e6

seedBrowserIndices <- paste0(stringi::stri_rand_strings(nbrowsers, 5, '[A-Z]'),
                             stringi::stri_rand_strings(nbrowsers, 4, '[0-9]'), 
                             stringi::stri_rand_strings(nbrowsers, 1, '[A-Z]'))

visitIDs <- paste0(stringi::stri_rand_strings(nsamples, 5, '[A-Z]'),
                   stringi::stri_rand_strings(nsamples, 4, '[0-9]'), 
                   stringi::stri_rand_strings(nsamples, 1, '[A-Z]'))

browserIDs <- sample(seedBrowserIndices, size = numSamples, replace = TRUE)

isconverted <- rbinom(nsamples, 1, 0.2)

searchVisits <- data.frame(browserIDs, isconverted, visitIDs)
colnames(searchVisits) <- c("BrowserID", "IsConverted", "VisitID")

N <- searchVisits %>% distinct(BrowserID) %>% nrow()

browserAllocation <- searchVisits %>% distinct(BrowserID) %>% 
                                      mutate(ABVariant = sample(c(0, 1), N, replace = TRUE))

#' *Count number of total visits and converting visits for A/B*
#' 

sVisits <- searchVisits %>% group_by(BrowserID) %>% 
                            mutate(TotalVisits = n(), 
                                   Converted = sum(IsConverted))


