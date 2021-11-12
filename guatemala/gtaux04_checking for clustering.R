sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS"))
sss_mi_dfs <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_mi_dfs.RDS"))

adult_life_x <- c("lifesat + children + married")  # Only in the COHORTS paper
  wealth_var = "pcall1618_1"
  sep_x <- paste0("+ pcall6775_1 + pcall1618_1 + eduyr + formal")
  early_life_x <- "moscho_sib + moage + male + chbirtho + gtatole + byear"
  rural = " + rural2016"
  adult_life_x = paste0(adult_life_x,rural)
  community_var = "adladdercommunity"
  economic_var = "adladdereconomic"
library(tidyverse)
for(outcome in c("bmi","srq","happiness")){
  formula3c_y <- paste0(outcome,"~ ",community_var," +",sep_x," +",early_life_x," +",adult_life_x,
                        "+ (1|d_id_unim)") %>% as.formula()
  
  lme4::lmer(formula3c_y,data=complete(sss_mi_dfs,2)) %>% 
    performance::icc(.) %>% 
    print(.)
}



