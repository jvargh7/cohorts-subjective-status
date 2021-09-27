

# Early life characteristics ------------

source(paste0(path_mobility_repo,"/guatemala/gtaux01_covariates.R"))

sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS"))  
sss_df %>% 
  dplyr::filter(!is.na(adladdercommunity),!is.na(adladdereconomic)) %>% 
  dplyr::select(id_uni,
                male,moscho_sib,moage,byear,chbirtho,gtatole,full) %>% 
  mutate(male = case_when(male == 1 ~ "Male",
                          male == 0 ~ "Female",
                          TRUE ~ NA_character_)) %>% 
  compareGroups::compareGroups(male~.-id_uni,data=.,
                               method = c(2,1,2,3,2,3,3)) %>% 
  compareGroups::createTable(.,show.all = TRUE,show.p.overall = FALSE,show.n = TRUE,sd.type = 2,q.type = c(2,2)) %>% 
  compareGroups::export2xls(.,file=paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss early life.xlsx"))


# Health outcomes ---------------

plot_df <- sss_df %>% 
  dplyr::select(id_uni,male,bmi,srq,happiness) %>% 
  pivot_longer(cols=-one_of("id_uni","male"),names_to="outcome",values_to="value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(male = case_when(male == 1 ~ "Male",
                          male == 0 ~ "Female",
                          TRUE ~ NA_character_),
         outcome = case_when(outcome == "bmi" ~ "BMI in 2016",
                             outcome == "srq" ~ "SRQ-20 in 2018",
                             outcome == "happiness" ~ "Happiness in 2018",
                             TRUE ~ NA_character_))

plot_df %>% 
  ggplot(data=.,aes(x=value,group=male,fill=male)) +
  geom_density(alpha=0.4) +
  facet_grid(~outcome,scales="free_x") +
  theme_bw() +
  xlab("") +
  ylab("Density") +
  scale_fill_discrete("") +
  theme(legend.position = "bottom")

