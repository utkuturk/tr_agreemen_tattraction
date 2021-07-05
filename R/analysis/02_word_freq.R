# word frequencies: 
# Utku: are these still relevant for us? 

word_freq <- readxl::read_excel("./data/frequencies_exp1_and_lago.xlsx", sheet = 1)
word_freq$freq_percentage %<>% as.numeric()
word_freq %<>% dplyr::select(-freq_standardized, -freq_percentage, -word)
word_freq %<>% tidyr::spread(place, freq_count)

word_freq %<>% dplyr::group_by(exp) %>% 
               dplyr::mutate( freq_cat_n1 = ifelse(n1 > median(n1), "high", "low"),
                              freq_cat_n2 = ifelse(n2 > median(n2), "high", "low"),
                              freqlog_n1 = scale(log(n1)),
                              freqlog_n2 = scale(log(n2)),
                              freqlog_n1n2 = log(n1) - log(n2)
                            ) %>% 
                ungroup()
# word_freq$freq_cat_n1 %<>% as.factor()
# word_freq$freq_cat_n2 %<>% as.factor()


word_freq_exp1 <- word_freq %>% subset(exp == "exp1") %>% dplyr::select(-exp)
df_merge_exp1 %<>% left_join(word_freq_exp1, by = "item")

word_freq_lago <- word_freq %>% subset(exp == "lagoetal") %>% dplyr::select(-exp)
df_merge_lago %<>% left_join(word_freq_lago, by = "item")
df_merge_lago$item %<>% add(1000)

df_merged <- dplyr::bind_rows(df_merge_exp1, df_merge_lago)
df_merged$subject %<>% as.factor()
df_merged$item %<>% as.factor()
