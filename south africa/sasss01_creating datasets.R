
source(paste0(path_mobility_repo,"/south africa/saaux01_covariates.R"))

ses_cs <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_cs.RDS")) %>% 
  arrange(year,bttid) %>% 
  dplyr::select(bttid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  dplyr::filter(!is.na(pc2018)|!is.na(pc2012))

# ANALYTIC SAMPLE -------
sss_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/south africa/absolute_df.RDS")) %>% 
  # Cannot include BMI since it was measured in 2012
  dplyr::select(-bmi,-fascho,-ravens,-nchildren,-starts_with("pc")) %>% 
  left_join(ses_cs,by="bttid") %>% 
  
  left_join(southafrica_dfa %>% 
              dplyr::select(bttid,adladdercommunity,adladdereconomic,adlifesat) %>% 
              rename(lifesat = adlifesat),
            by = "bttid") %>% 
  dplyr::filter(!is.na(adladdercommunity),!is.na(adladdereconomic)) %>% 
  dplyr::filter(!is.na(srq)|!is.na(happiness)) %>% 
  mutate_at(vars(children,formal,married,male,black), function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  mutate_at(vars(pc1990,pc1997,pc2002,pc2006,pc2012,pc2018
                 # moscho,moage,
                 # moht,
                 # eduyr,
                 # adladdercommunity,adladdereconomic
                 ),function(x) as.numeric(scale(x)))   %>% 
  mutate(eduyrC = scale(eduyr,scale=F) %>% as.numeric(.))

saveRDS(sss_df, paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS"))

# PATH DF ----------
path_df <- sss_df %>% 
  dplyr::select(bttid,srq,happiness, pc1990,pc1997,pc2002,pc2006,pc2012,pc2018,
                moscho,moage,black,male,formal,eduyr,chbirtho,
                adladdercommunity,adladdereconomic,
                lifesat,married,children) %>% 
  mutate_at(vars(male,black,formal,married,lifesat,children),~as.numeric(as.character(.))) %>% 
  mutate(moscho_imputed = case_when(is.na(moscho) ~ mean(moscho,na.rm=TRUE),
                                    TRUE ~ moscho),
         moage_imputed = case_when(is.na(moage) ~ mean(moage,na.rm=TRUE),
                                   TRUE ~ moage)) %>% 
  dplyr::select(-moscho,-moage) 


path_df %>% 
  compareGroups::compareGroups(~.-bttid,data=.) %>% 
  compareGroups::createTable(.)

saveRDS(path_df, paste0(path_dissertation,"/aim 3/working/cohorts/south africa/path_df.RDS"))

# MULTIPLE IMPUTATION FOR MULTIPLE REGRESSION
library(mice)

mi_iter = 10


mi_null <- mice(sss_df,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

# method[which(method == "pmm")] <- "rf"

# Each column is imputed by the rows
pred[,c("pregnant","bttid")] <- 0
pred[c("pregnant","bttid"),] <- 0
method["pregnant"] <- ""
method[c("eduyrC")] <- '~I(scale(eduyr,scale=F))'



mi_dfs <- mice(sss_df,
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=501)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_mi_dfs.RDS"))


