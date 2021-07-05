
fname_data <- "./data/exp_data.rds"
data_exp1 <- readRDS(fname_data)
fname_form <- "./data/exp_form.rds"
form_exp1 <- readRDS(file = fname_form)

### note: the low accuracy on fillers is not caused by a couple of items
###       -> see below
# filler_avgs <- 
# data_exp1 %>% subset(Type == "filler") %>% group_by(Item, condition) %>% 
#               summarize(M = mean(ResponseYes, na.rm = T)) %>%
#               arrange(condition, Item)
# fillers %>% subset(condition == "a") 
# fillers %>% subset(condition == "b")
###

# compute by-subject percentages of 'yes' responses, and average RTs 
avg_by_subj <- data_exp1 %>%
                group_by(subject, experiment, condition, 
                         grammatical, verb_num, attractor_num) %>%
                summarize(avRT = mean(RT), 
                          p_yes = mean(ResponseYes, na.rm = T), 
                          N = sum(!is.na(ResponseYes))  )

# reformat by-subject averages to a wide format
avg_by_subj_wide <- avg_by_subj %>% 
                      mutate(expcond = paste(experiment, condition, sep="_")) %>% 
                      ungroup() %>%
                      dplyr::select(-experiment, -condition, -avRT, -N,
                                    -grammatical, -verb_num, -attractor_num) %>%
                      tidyr::spread(expcond, p_yes) %>% 
                      mutate(delta_dc = AgrAttr_d - AgrAttr_c)

# Load Lago et al.'s monolingual data
fname_lagoetal <- "./dataraw/lagoetal/Lago_data.csv"
df_lagoetal <- read.csv(fname_lagoetal, encoding = "UTF-8", as.is = T)
df_lagoetal %<>% subset(Group == "monolingual")
df_lagoetal %<>% dplyr::select(-Accuracy, -L1:-Group, -List:-SelfRateGerman)

# Note: All rows with Experiment == "offline" also seem to be for 
#       the UNP task ('Grammatical' is NA). I wonder if these were
#       the same subjects, or if the subject labels were simply the same
#       for the two experiments.
with(df_lagoetal, stopifnot( is.na(Grammatical) == (Experiment == "offline") ))

df_lagoetal_unp <- df_lagoetal %>% 
                    subset(is.na(Grammatical)) %>%
                    dplyr::select(-Grammatical:-Label)
df_lagoetal_attr <- df_lagoetal %>% 
                  subset(!is.na(Grammatical)) %>%
                  dplyr::select(-Distance:-NewCond)

df_lagoetal_attr %<>% mutate(ResponseYes = (Response == "yes") ) %>% 
                      dplyr::select(-Response)
df_lagoetal_attr %<>% ungroup() %>%
                      dplyr::select(grammatical=Grammatical,
                                    attractor_num=Attractor,
                                    experiment=Experiment,
                                    lagoetal_condition=Condition, 
                                    subject=Participant, 
                                    item=Item,
                                    ResponseYes,
                                    RT)

# map to our condition labels
lagoetal_condition_mapping <- data.frame( condition = c("a", "b", "c", "d"),
                                          lagoetal_condition = c("d", "b", "c", "a"), 
                                          stringsAsFactors = F)
df_lagoetal_attr %<>% left_join( lagoetal_condition_mapping, by = "lagoetal_condition" )

# compute by-subject percentages of 'yes' responses, and average RTs 
avg_by_subj_lagoetal <- df_lagoetal_attr %>%
                            group_by(subject, experiment, condition, grammatical, attractor_num) %>%
                            summarize(avRT = mean(RT), 
                                      p_yes = mean(ResponseYes, na.rm = T), 
                                      N = sum(!is.na(ResponseYes))  )

# reformat by-subject averages to a wide format
avg_by_subj_lagoetal_wide <- avg_by_subj_lagoetal %>% 
                                    mutate(expcond = paste(experiment, condition, sep="_")) %>% 
                                    ungroup() %>%
                                    dplyr::select(-experiment, -condition, -avRT, -N,
                                                  -grammatical, -attractor_num) %>%
                                    tidyr::spread(expcond, p_yes) %>% 
                                    mutate(delta_dc = online_d - online_c)

n_subject <- data_exp1$subject %>% unique() %>% length()   
# identify bad participants 
accuracy_threshold <- 0.25
bad_subjects <- subset(avg_by_subj_wide, delta_dc <= accuracy_threshold ) %>% .$subject
data_exp1_clean <- data_exp1 %>% subset(!subject %in% bad_subjects)

rt_threshold <- 200
data_exp1_clean %<>% filter( RT > rt_threshold)

perc_deletion_exp1 <- round(100*((nrow(data_exp1)-nrow(data_exp1_clean))  / nrow(data_exp1)),2)

# identify bad participants 
bad_subjects_lagoetal <- subset(avg_by_subj_lagoetal_wide, delta_dc <= accuracy_threshold ) %>% .$subject
df_lagoetal_attr_clean <- df_lagoetal_attr %>% subset(!subject %in% bad_subjects_lagoetal)

df_lagoetal_attr_clean %<>% filter( RT > rt_threshold)

perc_deletion_lago <- round(100*((nrow(df_lagoetal_attr)-nrow(df_lagoetal_attr_clean))  / nrow(df_lagoetal_attr)),2)

# merge both datasets
df_merge_exp1 <- data_exp1_clean %>% ungroup() %>% 
                      dplyr::select(source=experiment, 
                                    grammatical, 
                                    attractor_num,
                                    # condition,
                                    subject, 
                                    item=Item,
                                    ResponseYes, 
                                    RT)
df_merge_exp1$experiment <- "Experiment 1"
df_merge_exp1$grammatical %<>% dplyr::recode(gram="grammatical", ungram="ungrammatical")
df_merge_exp1$attractor_num %<>% dplyr::recode(pl="plural", sg="singular")

df_merge_exp1$item %<>% as.integer()
df_merge_exp1$subject %<>% as.character()



df_merge_lago <- df_lagoetal_attr_clean %>%
                      ungroup() %>% 
                      dplyr::select(grammatical, attractor_num,
                                    subject, item, ResponseYes, RT)
df_merge_lago$experiment <- "Lago et al. (2018)" 
df_merge_lago$source <- NA
