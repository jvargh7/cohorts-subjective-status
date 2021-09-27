gt_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/all model coefs.csv")) %>% 
  dplyr::filter(term %in% c("pcall1618_1","formal1"),model %in% c("model3c","model3e")) %>% 
  dplyr::select(variable,term,model,coef_ci) %>% 
  pivot_wider(names_from="term",values_from="coef_ci") %>% 
  rename(formal = formal1,
         wealth_adult = pcall1618_1) %>% 
  mutate(site = "Guatemala") %>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/evalues.csv")) %>% 
              dplyr::select(model,variable,evalue),
            by=c("model","variable"))

ph_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/all model coefs.csv")) %>% 
  dplyr::filter(term %in% c("pc2018","formal1"),model %in% c("model3c","model3e")) %>% 
  dplyr::select(variable,term,model,coef_ci)  %>% 
  pivot_wider(names_from="term",values_from="coef_ci") %>%
  rename(formal = formal1,
         wealth_adult = pc2018) %>% 
  mutate(site = "Philippines") %>% 
  dplyr::filter(model %in% c("model3c","model3e"))%>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/evalues.csv")) %>% 
              dplyr::select(model,variable,evalue),
            by=c("model","variable"))

sa_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/all model coefs.csv")) %>% 
  dplyr::filter(term %in% c("pc2018","formal1"),model %in% c("model3c","model3e")) %>% 
  dplyr::select(variable,term,model,coef_ci)  %>% 
  pivot_wider(names_from="term",values_from="coef_ci") %>%
  mutate(site = "South Africa") %>% 
  rename(formal = formal1,
         wealth_adult = pc2018) %>% 
  dplyr::filter(model %in% c("model3c","model3e"))%>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/evalues.csv")) %>% 
              dplyr::select(model,variable,evalue),
            by=c("model","variable"))


tab3_df <- bind_rows(gt_df,
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
  dplyr::select(outcome,sss,site,wealth_adult,formal,evalue) %>% 
  pivot_wider(names_from=c("site"),values_from=c("wealth_adult","formal","evalue")) %>% 
  dplyr::select(outcome,sss,matches("Guatemala"),matches("Philippines"),matches("South Africa")) %>% 
  arrange(sss)
  

write.csv(tab3_df,paste0(path_dissertation,"/aim 3/working/cohorts/table3.csv"),row.names=FALSE)
