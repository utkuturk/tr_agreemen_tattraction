# Analysis

## Set seed
set.seed(42)

## Set the path to the active document (full_analysis_and_pdf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Set parameters for the model analysis
# parameters:
# By default, only run a model for 'acceptable responses 
# in which the data from exp1 and lago et al is pooled
# Change the following values to True if you would like to have
# additional exp1 only bayesian model and coefs
# additional lago only bayesian model and coes
# model of the RTs 

exp1only <- F; lagoonly <- F; rt <- F


# If else in which we check 
# First, the complete environment needed for the paper
# If the file does not exist, it runs all scripts and analyses
# saves the environment, which would already be present here.
# If the file exist, it asks whether or not you would like to refresh it.
# This would be done after some stuff within the analysis are changed 
# or the parameters above are changed.
# If said yes (y), then everything is run again.
# If said no (n), then it loads the complete_environment.RData
# Possible improvements:
## Check whether or not important stuff is in the complete_environment,
## or whether or not it is corrupt
## I put two files assignment, that should not be the case.

if (file.exists('data/complete_environment.RData') == F) {
  {
    
    ## Every file for the analysis and the paper is ordered nicely.
    files <<- c("./R/scripts/00.0-packages.R",
                "./R/scripts/00.1-prepare.R",
                "./R/scripts/00.2-eda.R",
                "./R/scripts/misc.R", 
                "./R/analysis/01_data_wrangling.R", "./R/analysis/02_word_freq.R", 
                "./R/analysis/03_descriptive_plots.R","./R/analysis/04_bayesian_models.R",
                "./R/analysis/05_model_plots.R", "./R/analysis/06_text_inputs.R")
    
    ## source them all
    for (f in files) {
      source(f)
    }
    
    ## save the image
    save.image(file='./data/complete_environment.RData')
  }
} else {
  wanna_refresh <<- readline(prompt = "You wanna refresh the environment? (y/n): ")
  if (wanna_refresh == 'y') {
    ## Every file for the analysis and the paper is ordered nicely.
    files <<- c("./R/scripts/00.0-packages.R",
                "./R/scripts/00.1-prepare.R",
                "./R/scripts/00.2-eda.R",
                "./R/scripts/misc.R", 
                "./R/analysis/01_data_wrangling.R", "./R/analysis/02_word_freq.R", 
                "./R/analysis/03_descriptive_plots.R","./R/analysis/04_bayesian_models.R",
                "./R/analysis/05_model_plots.R", "./R/analysis/06_text_inputs.R")
    
    ## source them all
    for (f in files) {
      source(f)
    }
    
    ## save the image
    save.image(file='./data/complete_environment.RData')
  } else {
    ## Load the data (may be really irrelevant but nice for not rerunning the whole thing)
    load("./data/complete_environment.RData")
  }
  
}

