
# Early life characteristics ------------

source(paste0(path_mobility_repo,"/south africa/saaux01_covariates.R"))

sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS"))  


sss_df %>% 
  dplyr::filter(!is.na(adladdercommunity),!is.na(adladdereconomic)) %>% 
  dplyr::select(bttid,male,moscho,moage,black,chbirtho) %>% 
  mutate(male = case_when(male == 1 ~ "Male",
                          male == 0 ~ "Female",
                          TRUE ~ NA_character_)) %>% 
  compareGroups::compareGroups(male~.-bttid,data=.,method = c(2,1,3,3)) %>% 
  compareGroups::createTable(.,show.all = TRUE,show.p.overall = FALSE,show.n = TRUE,sd.type = 2,q.type = c(2,2)) %>% 
  compareGroups::export2xls(.,file=paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss early life.xlsx"))



# Health Outcomes ------------------
plot_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/path_df.RDS")) %>% 
  dplyr::select(bttid,male,srq,happiness) %>% 
  pivot_longer(cols=-one_of("bttid","male"),names_to="outcome",values_to="value") %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(male = case_when(male == 1 ~ "Male",
                          male == 0 ~ "Female",
                          TRUE ~ NA_character_),
         outcome = case_when(outcome == "bmi" ~ "BMI in 2018",
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

