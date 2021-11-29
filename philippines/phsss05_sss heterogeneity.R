source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_pathanalysis.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))
source(paste0(path_sss_repo,"/package/coefs_heterogeneity.R"))
source(paste0(path_sss_repo,"/package/mi_reg_coef.R"))

sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_df.RDS"))
sss_mi_dfs <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/sss_mi_dfs.RDS"))

library(mice)
# y = "bmi"
# miaux_dfs = sss_mi_dfs
# het_var = "male"



consolidated_summary_table <- data.frame()
consolidated_contrasts_table <- data.frame()
consolidated_nested_table <- data.frame()

vars_het = c("male",
             "eduyrC",
             "pc2018"
)
vars_y = c(
  "bmi",
  "srq",
  "happiness"
)

for (y in vars_y) {
  for (h in vars_het){
    cat(y,":",h)
    
    # 95% CI using robust SE
    heterogeneity_out <- consolidated_heterogeneity(y,miaux_dfs = sss_mi_dfs,het_var = h,site="philippines")
    
    # Check using mice
    heterogeneity_out_mice <- mice_heterogeneity(y,miaux_dfs = sss_mi_dfs,het_var = h,site="philippines")
    
    temp_summary_table <- heterogeneity_out[[1]] %>% 
      mutate(y = y,
             h = h)
    temp_contrasts_table <- heterogeneity_out[[2]] %>% 
      mutate(y = y,
             h = h)
    
    temp_nested_table <- heterogeneity_out_mice[[2]] %>% 
      mutate(y = y,
             h = h)
    
    
    consolidated_summary_table <- bind_rows(consolidated_summary_table,
                                            temp_summary_table)
    
    consolidated_contrasts_table <- bind_rows(consolidated_contrasts_table,
                                              temp_contrasts_table)
    
    consolidated_nested_table <- bind_rows(consolidated_nested_table,
                                           temp_nested_table)
    
  }
  
  
  
}


consolidated_summary_table %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity summary table.csv"),row.names = FALSE)
consolidated_contrasts_table %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity contrasts table.csv"),row.names = FALSE)
consolidated_nested_table %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity nested table.csv"),row.names = FALSE)
