
source(paste0(path_replication_repo,"/package/e_values.R"))

sd_y <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS")) %>% 
  summarize_at(vars(bmi,srq,happiness),~sd(.,na.rm=TRUE)) %>% 
  pivot_longer(cols=everything(),names_to = "variable",values_to="sd") %>% 
  mutate(variable = case_when(variable == "bmi" ~ "BMI in 2018",
                              variable == "srq" ~ "SRQ-20 in 2018",
                              variable == "happiness" ~ "Happiness in 2018",
                              TRUE ~ NA_character_))




evalue_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  separate(coef_ci,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  dplyr::filter(model %in% c("model3c","model3e")) %>% 
  mutate(se = (coef - lci)/1.96) %>% 
  left_join(sd_y,by="variable") %>% 
  mutate(evalue = map(1:nrow(.),function(x) e_value_ci(coef[x],se[x],sd[x])[[4]]) %>% unlist()) %>% 
  arrange(model)

write.csv(evalue_df,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/evalues.csv"),row.names = FALSE)


