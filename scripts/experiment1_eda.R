library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

fname_data <- "../workspace_exp1/exp_data.rds"

data <- readRDS(file = fname_data)
dataAv <- data %>% 
  group_by(experiment, condition, grammatical, verb_num, attracted) %>%
  summarize(avRT = mean(RT), p_yes = mean(ResponseYes, na.rm = T)) %>% 
  as.data.frame()

dataAv %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attracted), color = experiment )) + geom_point() + geom_line(aes(linetype =attracted)) 

data$trial_group <- Hmisc::cut2(data$trial_no, g=5)

dataAvSplit <- data %>% 
  group_by(experiment, condition, grammatical, verb_num, attracted, trial_group) %>%
  summarize(avRT = mean(RT), p_yes = mean(ResponseYes, na.rm = T)) %>% 
  as.data.frame()

plot_split <- dataAvSplit %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attracted), color = experiment )) + geom_point() + geom_line(aes(linetype =attracted)) + facet_wrap(~trial_group)


dataAvClean <- slice(dataAvClean, 1:4)

plot_average <- dataAvClean %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attracted), color = experiment )) + geom_point() + geom_line(aes(linetype =attracted)) #+ facet_wrap(~experiment, scales = "free_y")
