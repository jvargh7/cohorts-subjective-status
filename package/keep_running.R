

# Creating datasets -----------
source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/guatemala/gtsss01_creating datasets.R"))
rm(list=ls())


source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/philippines/phsss01_creating datasets.R"))
rm(list=ls())


source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/south africa/sasss01_creating datasets.R"))
rm(list=ls())


# Run multiple regressions ------------

source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/guatemala/gtsss04_multiple regression.R"))
rm(list=ls())


source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/philippines/phsss04_multiple regression.R"))
rm(list=ls())

source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/south africa/sasss04_multiple regression.R"))
rm(list=ls())

# Run sss x sex -----------

source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/guatemala/gtsss05_sss x sex.R"))
rm(list=ls())


source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/philippines/phsss05_sss x sex.R"))
rm(list=ls())

source("C:/code/ses_transitions/sss/.Rprofile")
source(paste0(path_sss_repo,"/south africa/sasss05_sss x sex.R"))
rm(list=ls())
