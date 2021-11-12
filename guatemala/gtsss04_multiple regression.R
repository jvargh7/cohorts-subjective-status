

sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_df.RDS"))
sss_mi_dfs <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss_mi_dfs.RDS"))



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

result_path_all_model <- data.frame()

y_models <- list()

# y = "bmi"
for (y in c("bmi","srq","happiness")) {
  print(y)
  # temp_path_mr <- temp_mr(y,df = sss_mi_dfs,site = "guatemala")
  # y_models[y] <- temp_path_mr[[2]]
  
  temp_path_mr_mice <- mice_pool_reg(y,df = sss_mi_dfs,
                                     site = "guatemala",output_type="all_model_coefs")

  result_path_mr <- bind_rows(result_path_mr,
                              temp_path_mr_mice[[1]]
  )
  
  result_path_all_model <- bind_rows(result_path_all_model,
                              temp_path_mr_mice[[2]] %>% 
                                mutate(variable = y)
  )
  
}

result_path_mr <- result_path_mr %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(variable = case_when(
                              variable == "bmi" ~ "BMI in 2016",
                              variable == "srq" ~ "SRQ-20 in 2018",
                              variable == "lifesat" ~ "Life Satisfaction in 2018",
                              variable == "happiness" ~ "Happiness in 2018",
                              TRUE ~ variable
  ))

result_path_all_model <- result_path_all_model   %>% 
  mutate(variable = as.character(variable),
         lci = estimate - 1.96*std.error,
         uci = estimate + 1.96*std.error,
         coef_ci = paste0(round(estimate,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  mutate(variable = case_when(
    variable == "bmi" ~ "BMI in 2016",
    variable == "srq" ~ "SRQ-20 in 2018",
    variable == "lifesat" ~ "Life Satisfaction in 2018",
    variable == "happiness" ~ "Happiness in 2018",
    TRUE ~ variable
  ))

result_path_mr %>% 
  knitr::kable(format="markdown") 

result_path_mr %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/multiple regression coefficients.csv"),row.names = FALSE)
result_path_all_model %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/all model coefs.csv"),row.names = FALSE)

# saveRDS(y_models,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/sss multiple regression.RDS"))


