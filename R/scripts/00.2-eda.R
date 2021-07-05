fname_data <- "./data/exp_data.rds"

data <- readRDS(file = fname_data)
dataAv <- data %>% 
  group_by(experiment, condition, grammatical, verb_num, attractor_num) %>%
  summarize(avRT = mean(RT), p_yes = mean(ResponseYes, na.rm = T)) %>% 
  as.data.frame()

dataAv %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attractor_num), color = experiment )) + geom_point() + geom_line(aes(linetype =attractor_num)) 

data$trial_group <- Hmisc::cut2(data$trial_no, g=5)

dataAvSplit <- data %>% 
  group_by(experiment, condition, grammatical, verb_num, attractor_num, trial_group) %>%
  summarize(avRT = mean(RT), p_yes = mean(ResponseYes, na.rm = T)) %>% 
  as.data.frame()

plot_split <- dataAvSplit %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attractor_num), color = experiment )) + geom_point() + geom_line(aes(linetype =attractor_num)) + facet_wrap(~trial_group)


#dataAvClean <- slice(dataAvSplit, 1:4)

#plot_average <- dataAvClean %>% ggplot(aes(grammatical, p_yes, group = paste(experiment,attractor_num), color = experiment )) + geom_point() + geom_line(aes(linetype =attractor_num)) #+ facet_wrap(~experiment, scales = "free_y")
