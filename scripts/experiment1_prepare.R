library(languageR)
library(tidyverse)
library(gdata)
library(MASS)
library(magrittr)
library(ggplot2)

fname_data <- "../Data/Experiment1/results/results"
fname_data_out <- "../workspace_exp1/exp_data.rds"
fname_form_out <- "../workspace_exp1/exp_form.rds"
data <- read.csv(fname_data, 
                 header = F, 
                 comment.char = "#", 
                 encoding = "UTF-8" , 
                 col.names = paste0("V",seq_len(11)), 
                 fill = TRUE)
colnames(data) = c("Timestamp", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "Type", "Item", "Sentence", "Question","Answer", "RT")
data %<>% dplyr::select(-MD5)

FormEntries <- subset(data, ControllerType != "DashedAcceptabilityJudgment" ) %>% 
                      drop.levels(); 
nrow(data)
data %<>% subset(ControllerType == "DashedAcceptabilityJudgment")
age <- dplyr::select(FormEntries, Timestamp,Sentence,Question) %>% 
          dplyr::filter(Sentence == "age") %>%
          dplyr::select(Timestamp, Age = Question)
natturk <- dplyr::select(FormEntries, Timestamp,Sentence,Question) %>% 
          dplyr::filter(Sentence == "natturk") %>%
          dplyr::select(Timestamp, Natturk = Question) %>%
          mutate(Natturk = Natturk == "male")
dex <- dplyr::select(FormEntries, Timestamp,Sentence,Question) %>% 
          dplyr::filter(Sentence == "dex") %>%
          dplyr::select(Timestamp, Dex = Question) %>%
          mutate(Dex = ifelse(Dex == "male", "right-handed", "left-handed"))
FormPart <- dplyr::left_join(age, natturk, by = "Timestamp") %>% 
            dplyr::left_join(., dex , by = "Timestamp" ) %>% 
            unique()
lenForm <- length(levels(factor(FormPart$Timestamp)))
#md5form <- levels(factor(FormPart$MD5))
timestampForm <- levels(factor(FormPart$Timestamp))
timeform_subject <- data.frame("Timestamp" = timestampForm, "subject" = sprintf("S[%s]",seq(lenForm)))
FormPart <- merge(FormPart,timeform_subject, all = T)
FormPart$Age %<>% as.character() %>% gsub("on sekiz", "18", .) %>% as.integer()

data$Timestamp %<>% as.character()
timeform_subject$Timestamp %<>% as.character()

stopifnot( nrow(data) %% 2 == 0 )
rows_stim <- data[c(T,F),]
rows_resp <- data[c(F,T),]

data <- rows_resp %>% left_join(timeform_subject, by = "Timestamp") %>% 
          dplyr::select(-Timestamp, -ControllerType, -Sentence, -Element) %>%
          dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
          dplyr::select(-ResponseCorrect)
data %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
data %<>% within({ 
  late_response = (Response == "NULL")
  Response[late_response] = NA
})

responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
data$Response %<>% as.character() %>% enc2native()
stopifnot( all(data$Response %in% responses | is.na(data$Response) ) )

data$ResponseYes <- ifelse(grepl("P",data$Response) , T, 
                           ifelse(grepl("Q",data$Response) , F, NA))

data$condition <- 
  with(data, case_when(Type == "filler" & SentenceNoInStimFile >= 200 ~ "a",
                       Type == "filler" & SentenceNoInStimFile < 200 ~ "b",
                       Type == "condition_a" ~ "a",
                       Type == "condition_b" ~ "b",
                       Type == "condition_c" ~ "c",
                       Type == "condition_d" ~ "d",
                       TRUE ~ as.character(NA)))
data$experiment <- ifelse(data$Type %in% c("condition_a", "condition_b", "condition_c", "condition_d"), "AgrAttr", as.character(data$Type))


conditions_info <- data.frame(experiment =    c("AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "filler", "filler"),
                              condition =     c("a",      "b",    "c",      "d",    "a",      "b"),
                              grammatical =   c("ungram", "gram", "ungram", "gram", "ungram", "gram"),
                              verb_num =      c("pl",     "sg",   "pl",     "sg",   "sg",     "pl"),
                              attractor_num = c("pl",     "pl",   "sg",     "sg",    NA,       NA),
                              #attractor_match = c(T,      T,      F,        F,      F,        F),
                              stringsAsFactors = FALSE)
data %<>% left_join(conditions_info)
data %<>% subset(Type != "practice") %>% drop.levels()
saveRDS(FormPart, file = fname_form_out)
saveRDS(data, file = fname_data_out)

