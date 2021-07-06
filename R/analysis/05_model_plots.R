contrast_names <- c("cGrammatical" = "Grammaticality",
                    "cAttractorPlural" = "Plural Attactor",
                    "cGrammatical:cAttractorPlural" = "Grammaticality * Plural Attractor",
                    "cEndsInConsonant" = "Ambiguity",
                    "cEndsInConsonant:cGrammatical" = "Ambiguity * Grammaticality",
                    "cEndsInConsonant:cAttractorPlural" = "Ambiguity * Plural Attractor",
                    "cEndsInConsonant:cGrammatical:cAttractorPlural" = "Ambiguity * Grammaticality * Plural Attractor")

p_m_response <- 
  create_model_coefs_plot( m_responses, 
        plot_stats = T, map_names = contrast_names,
        expand_right = 2.5, expand_top = 1.4, x_stat_adjust = 1.1,
        x_breaks = -1:4 ) + 
        xlab("Estimate (probit)")

p_m_response <- p_m_response + theme(axis.title.x = element_text(hjust=0.15))

p_m_response <- print(p_m_response + annotate(x=-1, xend=4, y=0, yend=0, lwd=0.25, geom="segment"))

{
  if (exp1only ==T) {
    contrast_names_exp1 <- c("cGrammatical" = "Grammaticality",
                             "cAttractorPlural" = "Plural Attactor",
                             "freqlog_n1n2"="log Frequency: N1-N2",
                             "cGrammatical:freqlog_n1n2" = "Grammaticality * log Frequency: N1-N2",
                             "cAttractorPlural:freqlog_n1n2" = "Plural Attactor * log Frequency: N1-N2",
                             "cGrammatical:cAttractorPlural" = "Grammaticality * Plural Attractor",
                             "cGrammatical:cAttractorPlural:freqlog_n1n2" = "Grammaticality * Plural Attractor * log Frequency: N1-N2"
    )
    
    p_m_response_exp1 <- 
      create_model_coefs_plot( m_responses_exp1only, 
                               plot_stats = T, map_names = contrast_names_exp1,
                               expand_right = 2.5, expand_top = 2, x_stat_adjust = 1.1,
                               x_breaks = -1:4 ) + 
      xlab("Estimate (probit)")
  }
}




{
  if (lagoonly == T) {
    contrast_names_exp1 <- c("cGrammatical" = "Grammaticality",
                             "cAttractorPlural" = "Plural Attactor",
                             "freqlog_n1n2"="log Frequency: N1-N2",
                             "cGrammatical:freqlog_n1n2" = "Grammaticality * log Frequency: N1-N2",
                             "cAttractorPlural:freqlog_n1n2" = "Plural Attactor * log Frequency: N1-N2",
                             "cGrammatical:cAttractorPlural" = "Grammaticality * Plural Attractor",
                             "cGrammatical:cAttractorPlural:freqlog_n1n2" = "Grammaticality * Plural Attractor * log Frequency: N1-N2"
    )
    p_m_response_lago <- 
      create_model_coefs_plot( m_responses_Lagoonly, 
                               plot_stats = T, map_names = contrast_names_exp1,
                               expand_right = 2.5, expand_top = 2, x_stat_adjust = 1.1,
                               x_breaks = -2:4 ) + 
      xlab("Estimate (probit)")
  }
}



{
  if (exp1only == T & lagoonly ==T) {
    p_m_response_exp1 <- p_m_response_exp1 + facet_wrap(~"Experiment 1") + annotate(x=-2, xend=4, y=0, yend=0, lwd=0.25, geom="segment")
    
    
    p_m_response_lago <- p_m_response_lago + facet_wrap(~"Lago et al. (2018)") + annotate(x=-2, xend=4, y=0, yend=0, lwd=0.25, geom="segment")
    
    p_m_response_exp1_and_lago <- ggarrange(p_m_response_exp1, p_m_response_lago, ncol = 1)
    
  }
}


{
  if (rt == T) {
    # TODO: fill this up.
  }
}

{
  ungram_contrast_names <- c("cEndsInConsonant" = "Ambiguity",
                             "cAttractorPlural" = "Plural Attactor",
                             "cEndsInConsonant:cAttractorPlural" = "Ambiguity * Plural Attractor")
  
  p_m_ungram_response <- 
    create_model_coefs_plot( m_ungram_responses, 
                             plot_stats = T, map_names = ungram_contrast_names,
                             expand_right = 2, expand_top = 5, x_stat_adjust = 1.1,
                             x_breaks = c(-1,-0.5,0,0.5,1) ) + 
    xlab("Estimate (probit)")
  
  p_m_ungram_response <- p_m_ungram_response + theme(axis.title.x = element_text(hjust=0.25))
  
  p_m_ungram_response <- print(p_m_ungram_response + annotate(x=-1, xend=1, y=0, yend=0, lwd=0.25, geom="segment"))
  
}
