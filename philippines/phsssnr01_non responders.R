
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

interviewstat_2018 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2018/interviewstat-2018.dta"))

status2009 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey == 2009) %>% 
  
  dplyr::filter(uncchdid < 23310)


died2009_ids = status2009 %>% 
  dplyr::filter(crnoint == 10) %>% 
  dplyr::select(uncchdid) %>% 
  pull()

died2018_ids = interviewstat_2018 %>% 
  dplyr::filter(intstat2018 == "died") %>% 
  dplyr::select(uncchdid) %>% 
  pull()




# cstratum
# 1	Urban
# 2	Rural
ph_region <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey %in% c(0.5)) %>% 
  dplyr::filter(!is.na(cstratum)) %>% 
  dplyr::select(uncchdid,cstratum)

ses_cs <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_region.RDS")) %>% 
  dplyr::select(uncchdid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  dplyr::select(uncchdid,pc1983) %>% 
  left_join(ph_region %>% 
              rename(cstratum1983 = cstratum),
            by = "uncchdid")

alive_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/early_life.RDS")) %>% 
  left_join(ses_cs,by = "uncchdid") %>% 
  mutate_at(vars(cstratum1983,pc1983),~case_when(is.na(.) ~ Mode(.),
                                                 TRUE ~ .)) %>% 
  mutate(alive2018 = case_when(uncchdid %in% unique(c(died2018_ids,
                                                    died2009_ids)) ~ 0,
                               TRUE ~ 1))




rhs_formula_a = "~ chsex + moscho + moage + pc1983 + cstratum1983 + chbirtho"

alive_df$censoring_weight = censoring_weights(c_formula = paste0("alive2018", rhs_formula_a),
                                              df = alive_df,
                                              type = "glm")
saveRDS(alive_df,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/alive_df.RDS"))

# Participation weight in SSS -------------
rhs_formula_s = "~ chsex + moscho + moage + pc1983 + cstratum1983 + chbirtho"
sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS"))

provided_sss_df <- alive_df %>% 
  dplyr::filter(alive2018 == 1) %>% 
  mutate(provided_sss = case_when(uncchdid %in% sss_df$uncchdid ~ 1,
                                  TRUE ~ 0)) %>% 
  left_join(sss_df %>% 
              dplyr::select(uncchdid,eduyr,pc2018,
                            rural2018,married),
            by = "uncchdid")

provided_sss_df$sss_weight = censoring_weights(c_formula = paste0("provided_sss", rhs_formula_s),
                                        df = provided_sss_df,
                                        type = "glm")


saveRDS(provided_sss_df,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/provided_sss_df.RDS"))



