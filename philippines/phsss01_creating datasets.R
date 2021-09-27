source(paste0(path_gains_repo,"/philippines/phwgaux01_wellbeing total.R"))

philippines_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "cebu") %>% 
  dplyr::mutate(uncchdid = pin - 40000000) 

ses_cs <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_region.RDS")) %>% 
  dplyr::select(uncchdid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc")

# ANALYTIC SAMPLE -------
sss_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/philippines/absolute_df.RDS")) %>%
  dplyr::select(-starts_with("pc"),-ravens,-happiness,
                -rural1991,-rural1994,-rural1998,
                -rural2002,-rural2005,-rural2007,-rural2009) %>% 
  left_join(ses_cs,
            by="uncchdid") %>% 
  left_join(philippines_dfa %>% 
              dplyr::select(uncchdid,adladdercommunity,adladdereconomic),
            by = "uncchdid") %>% 
  # Bringing in wellbeing and life satisfaction separately
  left_join(ph_wellbeing %>% 
              dplyr::select(uncchdid,d_happiness_tot_imp) %>% 
              rename(happiness = d_happiness_tot_imp),
            by=c("uncchdid")) %>% 
  left_join(ph_lifesat %>% 
              dplyr::select(uncchdid,d_gen_life_sat_tot_imp) %>% 
              rename(lifesat = d_gen_life_sat_tot_imp),
            by=c("uncchdid")) %>% 
  dplyr::filter(!is.na(adladdercommunity),!is.na(adladdereconomic)) %>% 
  dplyr::filter(!is.na(bmi)|!is.na(srq)|!is.na(happiness)) %>% 
  mutate_at(vars(children,formal,married,male,contains("rural")), function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  mutate_at(vars(pc1983,pc1991,pc1994,pc1998,pc2002,pc2005,pc2009,pc2018
                 # moscho,moage,
                 # moht,
                 # eduyr,
                 # adladdercommunity,adladdereconomic
                 ),function(x) as.numeric(scale(x)))  %>% 
  mutate(eduyrC = scale(eduyr,scale=F) %>% as.numeric(.))

sss_df %>% 
compareGroups::compareGroups(~.-uncchdid,data=.) %>% 
  compareGroups::createTable(.)

saveRDS(sss_df, paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS"))

# PATH DF ------------
path_df <- sss_df %>% 
  mutate_at(vars(male,contains("rural"),formal),~as.numeric(as.character(.))) %>% 
  dplyr::select(uncchdid,pc1983,pc1991,pc1994,pc1998,pc2002,pc2005,pc2009,pc2018,
                rural2018,
                moscho,moage,chbirtho,formal,male,bmi,srq,happiness,
                # moht,
                eduyr,
                adladdercommunity,adladdereconomic,lifesat,married,children)


saveRDS(path_df, paste0(path_dissertation,"/aim 3/working/cohorts/philippines/path_df.RDS"))

# MULTIPLE IMPUTATION FOR MULTIPLE REGRESSION
library(mice)

mi_iter = 10


mi_null <- mice(sss_df,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

# Each column is imputed by the rows
pred[,c("pregnant","uncchdid")] <- 0
pred[c("pregnant","uncchdid"),] <- 0
method[c("pregnant","uncchdid")] <- ""
method[c("eduyrC")] <- '~I(scale(eduyr,scale=F))'



# method["formal"] <- "logreg"

mi_dfs <- mice(sss_df,
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=501)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_mi_dfs.RDS"))
