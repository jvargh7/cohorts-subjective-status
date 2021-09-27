source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

source(paste0(path_mobility_repo,"/south africa/saaux01_covariates.R"))

participated2018_ids = southafrica_dfa %>% 
  dplyr::filter(!is.na(adagey)) %>% 
  dplyr::select(bttid) %>% 
  pull()

ses_cs <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_cs.RDS")) %>% 
  dplyr::select(bttid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  dplyr::select(bttid,pc1990)

alive_df <- early_life %>% 
  left_join(ses_cs,by = "bttid") %>% 
  mutate_at(vars(pc1990,moage,moscho),~case_when(is.na(.) ~ Mode(.),
                                                 TRUE ~ .)) %>% 
  mutate(participated2018 = case_when(bttid %in% unique(participated2018_ids) ~ 1,
                               TRUE ~ 0))



# We don't have data on life status in South Africa --------
alive_df$censoring_weight = 1 
rhs_formula_a = "~ chsex + moscho + moage + pc1990 + ethnicity + chbirtho"

alive_df$participation_weight = censoring_weights(c_formula = paste0("participated2018", rhs_formula_a),
                                                  df = alive_df,
                                                  type = "glm")
saveRDS(alive_df,paste0(path_dissertation,"/aim 3/working/cohorts/south africa/alive_df.RDS"))

# Participation weight in SSS -------------
rhs_formula_s = "~ chsex + moscho + moage + pc1990 + ethnicity + chbirtho"
sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS"))

provided_sss_df <- alive_df %>% 
  # dplyr::filter(alive2018 == 1) %>% 
  mutate(provided_sss = case_when(bttid %in% sss_df$bttid ~ 1,
                                  TRUE ~ 0))

provided_sss_df$sss_weight = censoring_weights(c_formula = paste0("provided_sss", rhs_formula_s),
                                               df = provided_sss_df,
                                               type = "glm")

saveRDS(provided_sss_df,paste0(path_dissertation,"/aim 3/working/cohorts/south africa/provided_sss_df.RDS"))
