gt_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/multiple regression coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  mutate(site = "Guatemala") %>% 
  dplyr::filter(model %in% c("model1c","model1e",
                             "model2c","model2e",
                             "model3c","model3e"))

ph_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  mutate(site = "Philippines") %>% 
  dplyr::filter(model %in% c("model1c","model1e",
                             "model2c","model2e",
                             "model3c","model3e"))

sa_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/multiple regression coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  mutate(site = "South Africa") %>% 
  dplyr::filter(model %in% c("model1c","model1e",
                             "model2c","model2e",
                             "model3c","model3e"))



tab2_df <- bind_rows(gt_df,
                     ph_df,
                     sa_df) %>% 
  mutate(outcome = case_when(str_detect(variable,"BMI") ~ "BMI (kg/m2)",
                             str_detect(variable,"SRQ-20") ~ "WHO SRQ-20",
                             str_detect(variable,"Happiness") ~ "Subjective Happiness Scale",
                             TRUE ~ NA_character_
  ),
  sss = case_when(str_detect(model,"c$") ~ "Perceived Community Respect",
                  str_detect(model,"e$") ~ "Perceived Economic Status",
                  TRUE ~ NA_character_)) %>% 
  dplyr::select(outcome,sss,site,coef_ci,model) %>% 
  pivot_wider(names_from=c("site","model"),values_from=c("coef_ci")) %>% 
  dplyr::select(outcome,sss,matches("Guatemala"),matches("Philippines"),matches("South Africa")) %>% 
  arrange(sss)


write.csv(tab2_df,paste0(path_dissertation,"/aim 3/working/cohorts/table2.csv"),row.names=FALSE)
