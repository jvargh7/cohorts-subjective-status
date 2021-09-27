source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_pathanalysis.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))

sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_df.RDS"))
sss_mi_dfs <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss_mi_dfs.RDS"))

# Running models ------------

source(paste0(path_sss_repo,"/package/mi_reg_coef.R"))

result_path_mr <- data.frame(
  variable = character(),
  model1c = character(),
  model2c = character(),
  model3c = character(),
  model4Ac = character(),
  model4Bc = character(),
  model4Cc = character(),
  
  model1e = character(),
  model2e = character(),
  model3e = character(),
  model4Ae = character(),
  model4Be = character(),
  model4Ce = character()
)


for (y in c("srq","happiness")) {
  
  print(y)
  # temp_path_mr <- temp_mr(y,df = sss_mi_dfs,site = "south africa")
  # y_models[y] <- temp_path_mr[[2]]
  
  temp_path_mr_mice <- mice_pool_reg(y,df = sss_mi_dfs,site = "south africa",
                                     all_wealth = TRUE)
  
  
  result_path_mr <- bind_rows(result_path_mr,
                              temp_path_mr_mice
  )
  
  
}

result_path_mr <- result_path_mr %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(variable = case_when(
    variable == "bmi" ~ "BMI in 2012",
    variable == "srq" ~ "SRQ-20 in 2018",
    variable == "lifesat" ~ "Life Satisfaction in 2018",
    variable == "happiness" ~ "Happiness in 2018",
    TRUE ~ variable
  ))


result_path_mr %>% 
  knitr::kable(format="markdown") 

result_path_mr %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/south africa/multiple regression all wealth coefficients.csv"),row.names = FALSE)

# saveRDS(y_models,paste0(path_dissertation,"/aim 3/working/cohorts/south africa/sss multiple regression.RDS"))


