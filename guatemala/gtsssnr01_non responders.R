# Similar to ses_transitions/cohorts_replication/lca/ipaw for alive.R
# Used in Varghese 2021 SSMPH mobility paper
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))


gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))
alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()
alive2016 <- gates %>% 
  dplyr::filter(edo_vida2015 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

source(paste0(path_incap_repo,"/structural/early_life.R"))

ses_cs <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/ses_cs for COHORTS.RDS")) %>% 
  arrange(id_uni)


alive_df <- incap_early_life %>% 
  left_join(ses_cs %>% 
              dplyr::select(id_uni,pcall6775_1),
            by = "id_uni") %>% 
  mutate(alive2018 = case_when(id_uni %in% alive2018 ~ 1,
                               TRUE ~ 0),
         alive2016 = case_when(id_uni %in% alive2016 ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(exposure1000 = case_when(gtchatoleexposurestatus %in% c("partial","none") ~ 0,
                                  TRUE ~ 1))
# Censoring weight --------
rhs_formula_a = "~ chsex + moscho_imputed + moage_imputed + gtatole + gtchbyear + pcall6775_1"
alive_df$c_alive2018 = censoring_weights(c_formula = paste0("alive2018", rhs_formula_a),
                                         df = alive_df,
                                         type = "glm")

alive_df$c_alive2016 = censoring_weights(c_formula = paste0("alive2016", rhs_formula_a),
                                         df = alive_df,
                                         type = "glm")
saveRDS(alive_df,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/alive_df.RDS"))


# Participation weight in SSS -------------
rhs_formula_s = "~ chsex + moscho_imputed + moage_imputed + gtatole + gtchbyear + pcall6775_1"
sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS"))

provided_sss_df <- alive_df %>% 
  dplyr::filter(alive2018 == 1) %>% 
  mutate(provided_sss = case_when(id_uni %in% sss_df$id_uni ~ 1,
                                  TRUE ~ 0))

provided_sss_df$sss_weight = censoring_weights(c_formula = paste0("provided_sss", rhs_formula_s),
                                        df = provided_sss_df,
                                        type = "glm")


saveRDS(provided_sss_df,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/provided_sss_df.RDS"))
# print(outcome_alive)
