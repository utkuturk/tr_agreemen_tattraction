# data, code, and paper for the short turkish agreement attraction paper
[@utkuturkling](https://twitter.com/utkuturkling) & [@pavellogacev](https://twitter.com/pavellogacev)

## Run Order and Details
- First open [full_analysis.R](full_analysis.R) file run everything inside. 
  - It includes three parameters: additional standalone Exp1 analysis, additionalsStandalone Lago et al analysis, additional standalone rt analysis.
- All of the models and the data will be present inside the [data/complete_environment.RData](data/complete_environment.RData).
- Then, run [pdf_render.R](pdf_render.R). It converts the Sweave file into a knitr file and then render a pdf out of knitr file.
- Stand-alone scripts and data analysis can be found in R directory.
  - The ordering between them as follows: [packages](R/scripts/00.0-packages.R), [read_ibex](R/scripts/00.1-prepare.R), [eda](R/scripts/00.2-eda.R), [misc_scripts](R/scripts/misc.R), [data_wrangling](R/analysis/01_data_wrangling.R), [word_frequencies](R/analysis/02_word_freq.R), [descriptive_plots](R/analysis/03_descriptive_plots.R), [bayesian_models](R/analysis/04_bayesian_models.R), [model_plots](R/analysis/05_model_plots.R), [text_inputs](R/analysis/06_text_inputs.R)
- Stan models exported from the brms package can be found in [R/stan/](R/stan/) folder.
- Raw data from both Lago et al. (2019) and our experiment can be found in [dataraw/](dataraw/).
- Complete environment needed for the paper, experimental data, and the demographic information of the experiment can be found in [data/](data/)
- The glossa journal template and our sweave/knitr files can be found in [paper/glossa/](paper/glossa/).
- Edits should be done in [glossa-template.Rnw](paper/glossa/glossa-template.Rnw) file.
- Our [PDF](paper/glossa/glossa-template-knitr.pdf) is also present in [paper/glossa/](paper/glossa/)
- We use [Glossalatex](https://github.com/guidovw/Glossalatex) resources with small changes which is introduced due to [Glossa-Psycholinguistics guidelines](https://escholarship.org/uc/glossapsycholinguistics/structure_of_submission)
- For automated word count, we use [this solution](https://tex.stackexchange.com/a/239703) by [Omar Wasow](https://tex.stackexchange.com/users/34597/omar-wasow)using Texcount Perl script. In addition to this solution, we excluded unnumbered parts automatically.

## Requirements

List of packages, softwares, fonts you need for replicating our paper and models:
* LaTeX
* Brill Fonts
* Fira Sans Fonts
* R
* Rstudio (needed for setwd to an active document)
* Stan
* and following R packages
  * languageR
  * tidyverse
  * gdata
  * MASS
  * magrittr
  * ggplot2
  * car
  * brms
  * xtable
  * rstan
  * knitr
  * dplyr
  * plyr