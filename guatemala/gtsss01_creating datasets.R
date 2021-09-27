
source(paste0(path_mobility_repo,"/guatemala/gtaux01_covariates.R"))
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

ses_cs <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/ses_cs for COHORTS.RDS"))

# ANALYTIC SAMPLE ----------
sss_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts/guatemala/absolute_df.RDS")) %>% 
  dplyr::select(-starts_with("pcall"),-ravens,-rural,-nchildren,-gtvillage) %>% 
  left_join(ses_cs %>% 
              mutate(pcall1618_1 = case_when(!is.na(pcall2016_1) & !is.na(pcall2018_1) ~ rowMeans(.[,c("pcall2016_1","pcall2018_1")],na.rm=TRUE),
                                             !is.na(pcall2016_1) ~ pcall2016_1,
                                             !is.na(pcall2018_1) ~ pcall2018_1,
                                             TRUE ~ NA_real_)) %>% 
              dplyr::select(id_uni,pcall6775_1,
                            pcall1987_1,pcall1996_1,
                            pcall2002_1,pcall1618_1),
            by = "id_uni") %>% 
  left_join(guatemala_dfa %>% 
              dplyr::select(id_uni,adladdercommunity,adladdereconomic,adlifesat)  %>% 
              rename(lifesat = adlifesat),
            by = "id_uni") %>% 
  # Filtering for analytic sample ------
  dplyr::filter(!is.na(adladdercommunity),!is.na(adladdereconomic)) %>% 
  dplyr::filter(!is.na(bmi)|!is.na(srq)|!is.na(happiness)) %>%
  mutate_at(vars(children,formal,contains("rural")), function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  mutate_at(vars(pcall6775_1,pcall1987_1,
                 pcall1996_1,pcall2002_1,pcall1618_1
                 # moscho,moage,
                 # moht,
                 # eduyr,
                 # adladdercommunity,adladdereconomic
                 ),
            function(x) as.numeric(scale(x))) %>% 
  arrange(d_id_unim) %>% 
  mutate(eduyrC = scale(eduyr,scale=F) %>% as.numeric(.))

saveRDS(sss_df, paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS"))


# PATH DF --------------
path_df <- sss_df %>% 
  left_join(early_life %>% 
              dplyr::select(id_uni,moscho_imputed,moage_imputed,gtvillage) %>% 
              mutate_at(vars(moscho_imputed,moage_imputed),~scale(.)),
            by="id_uni") %>% 
  mutate(atolefull = case_when(gtatole == 1 & full == 1 ~ 1,
                               gtatole == 0 | full == 0 ~ 0,
                               TRUE ~ NA_real_)) %>% 
  mutate_at(vars(rural2016,rural2018,formal,married),~as.numeric(as.character(.))) %>% 
  dplyr::select(d_id_unim,id_uni,gtvillage,bmi,srq,happiness, 
                pcall1618_1,formal,rural2016,rural2018,
                pcall2002_1,pcall1996_1,pcall1987_1,eduyr,
                pcall6775_1,moscho_imputed,gtatole,full,
                atolefull,moage_imputed,chbirtho, male,
                byear,adladdercommunity,adladdereconomic,
                children,lifesat,married) %>% 
  group_by(gtvillage) %>% 
  mutate(chbirtho_imputed = case_when(is.na(chbirtho) ~ Mode(chbirtho),
                                      TRUE ~ chbirtho)) %>% 
  ungroup()


path_df %>% 
  compareGroups::compareGroups(~.-id_uni-d_id_unim,data=.) %>% 
  compareGroups::createTable(.)

saveRDS(path_df, paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/path_df.RDS"))


# MULTIPLE IMPUTATION FOR MULTIPLE REGRESSION
library(mice)

mi_iter = 10


mi_null <- mice(sss_df,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

# Each column is imputed by the rows
pred[,c("pregnant","id_uni","d_id_unim")] <- 0
pred[c("pregnant","id_uni","d_id_unim"),] <- 0
method[c("pregnant","id_uni","d_id_unim")] <- ""
method[c("eduyrC")] <- '~I(scale(eduyr,scale=F))'

mi_dfs <- mice(sss_df,
                  method = method,
                  pred = pred,
                  m=mi_iter,maxit=50,seed=501)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_mi_dfs.RDS"))
