df_exp1_na_nofillers <- subset(df_merge_exp1, is.na(ResponseYes) & source != "filler")
df_merged %<>% subset( !is.na(ResponseYes) )

df_merged %<>% mutate(ResponseCorrect = (ResponseYes == (grammatical == "grammatical") ) )
df_merged_nonna <- df_merged %>% subset(!is.na(ResponseYes))

# avg_clean <- 
# df_merged %>% subset(experiment == "Lago et al. (2018)") %>%
#      group_by(experiment, source, grammatical, attractor_num, subject) %>%
#      summarize(p_yes = mean(ResponseYes, na.rm=T)) %>%
#      summarize(p_yes = mean(p_yes))

avg_clean <- list()
avg_clean$resp <- df_merged_nonna %>% 
              plyr::ddply(c("experiment"), function(df) {
              df %>% se_cousineau(n_conditions = 4, subject, DV = ResponseYes, 
                           group = c("experiment", "source", "grammatical", "attractor_num"), 
                           is_proportion = TRUE)
})

avg_clean$rt <- df_merged_nonna %>%
              plyr::ddply(c("experiment"), function(df) {
              df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                           group = c("experiment", "source", "grammatical", "attractor_num"), 
                           is_proportion = FALSE)
})

avg_clean$rt_correct <- df_merged_nonna %>% subset(ResponseCorrect) %>%
              plyr::ddply(c("experiment"), function(df) {
              df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                           group = c("experiment", "source", "grammatical", "attractor_num"), 
                           is_proportion = FALSE)
})

avg_exp <- avg_clean %>% lapply(function(df) { df %>% subset(is.na(source) | source != "filler") })
avg_fillers <- avg_clean %>% lapply(function(df) { df %>% subset(source == "filler") })

avg_exp$resp$grammatical_plot <- ifelse(avg_exp$resp$grammatical == 'grammatical', 
                                   'Grammatical\n(Singular Verb)',
                                   'Ungrammatical\n(Plural Verb)')
pd <- position_dodge(0.0)
p_avg_resp <- avg_exp$resp %>%
              ggplot(aes(grammatical_plot, M, #linetype = attractor_num, 
                         color = attractor_num, group = attractor_num)) + 
                geom_point(position = pd) + geom_line(position = pd) + 
                facet_wrap(~experiment)

p_avg_resp <- p_avg_resp + geom_errorbar(aes(ymin = M - 1.96*SE, ymax = M + 1.96*SE), width = 0.1, position = pd)

# p_avg_resp <- p_avg_resp + geom_line(data = avg_fillers) + 
#                             geom_point(data = avg_fillers) + 

p_avg_resp <- p_avg_resp + theme( strip.background = element_rect(fill="white") ) +
                           theme_bw(base_family = "Fira Sans") + xlab("") + ylab("Percentage 'acceptable'")
p_avg_resp <- p_avg_resp + scale_y_continuous(labels=scales::percent)#, breaks = c(0, .25, .5, .75, 1))
p_avg_resp <- p_avg_resp + theme_bw(base_family = "Fira Sans")
p_avg_resp <- p_avg_resp + scale_color_discrete(name = "Attractor Number")

avg_exp_resp_exp1 <- avg_exp$resp %>% subset(experiment == "Experiment 1")
avg_exp_resp_lagoetal <- avg_exp$resp %>% subset(experiment == "Lago et al. (2018)")
