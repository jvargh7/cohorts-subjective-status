sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS"))
sss_mi_dfs <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_mi_dfs.RDS"))

adult_life_x <- c("lifesat + children + married")  # Only in the COHORTS paper
wealth_var = "pc2018"
sep_x <- "+ pc1983 + pc2018 + eduyr + formal"
rural = " + rural2018"
early_life_x <- "moscho + moage + male + chbirtho + rural1983"
adult_life_x = paste0(adult_life_x,rural)
community_var = "adladdercommunity"
economic_var = "adladdereconomic"
library(tidyverse)
for(outcome in c("bmi","srq","happiness")){
  formula3c_y <- paste0(outcome,"~ ",community_var," +",sep_x," +",early_life_x," +",adult_life_x,
                        "+ (1|currbrgy)") %>% as.formula()
  
  lme4::lmer(formula3c_y,data=complete(sss_mi_dfs,2)) %>% 
    performance::icc(.) %>% 
    print(.)
}