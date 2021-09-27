
gt_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS")) %>% 
  rename(id = id_uni) %>% 
  dplyr::select(-d_id_unim,-contains("pcall")) %>% 
  rename(moscho = moscho_sib)
ph_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS")) %>% 
  rename(id = uncchdid) %>% 
  dplyr::select(-contains("pc")) %>% 
  mutate(male = as.character(male) %>% as.numeric(.))
sa_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS")) %>% 
  rename(id = bttid) %>% 
  dplyr::select(-contains("pc")) %>% 
  mutate(male = as.character(male) %>% as.numeric(.))


table1_df <- bind_rows(gt_df %>% 
                         mutate(site = "Guatemala"),
                       ph_df %>% 
                         mutate(site = "Philippines"),
                       sa_df %>% 
                         mutate(site = "South Africa")) %>% 
  mutate(bmi = case_when(pregnant == 1 ~ NA_real_,
                         TRUE ~ bmi))


# CompareGroups ---------

library(compareGroups)
 
  compareGroups(site ~ adladdercommunity + adladdereconomic +
                  moscho + eduyr + formal +
                  moage + chbirtho + male +
                  byear + rural1983 + black +
                  married +
                  children + pregnant + rural2018 + lifesat +
                  bmi + srq + happiness,
                method = c(2,2,
                           2,2,3,
                           2,2,3,
                           2,3,3,
                           3,
                           3,3,3,2,
                           1,2,2)
                  ,data=table1_df) %>% 
    createTable(.,digits=1,show.all=FALSE,q.type = c(2,2),sd.type = 2) %>% 
    export2xls(.,paste0(path_dissertation,"/aim 3/working/cohorts/table 1 compareGroups.xlsx"))


# Arsenal ---------

library(arsenal)
  table1_control = tableby.control(test=F,total=F,
                                   numeric.stats = c("meansd","medianq1q3","N","Nmiss2"),
                                   cat.stats = c("countpct","N","Nmiss2"),
                                   stats.labels = list(
                                     meansd = "'Mean (SD)",
                                     medianq1q3 = "'Median (Q1, Q3)",
                                     N = "'N",
                                     Nmiss2 = "'Missing"
                                   ))
  table1_df %>% 
    mutate_at(vars(formal,male,rural1983,black,
                   children,married,rural2018,pregnant),~as.character(.)) %>% 
    tableby(site ~ adladdercommunity + adladdereconomic +
              moscho + eduyr + formal +
              moage + chbirtho + male +
              byear + rural1983 + black +
              married +
              children + rural2018 + lifesat +
              bmi + srq + happiness,data=.,
            control=table1_control,digits=1, digits.p=2, digits.pct=1) %>% 
    summary(.,text=TRUE) %>% 
    xlsx::write.xlsx(.,paste0(path_dissertation,"/aim 3/working/cohorts/table 1 arsenal.xlsx"),sheetName = "arsenal table1")
  