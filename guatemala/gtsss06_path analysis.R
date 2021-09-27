library(lavaan)

source(paste0(path_mobility_repo,"/guatemala/gtaux01_covariates.R"))
source(paste0(path_replication_repo,"/package/clean_pathanalysis.R"))


source(paste0(path_sss_repo,"/guatemala/gtaux01_renaming variables.R"))
source(paste0(path_sss_repo,"/guatemala/gtaux02_path models.R"))
path_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/path_df.RDS"))

# BMI -------------

result_model_bmi1 <- sem(path_model_bmi1,
                         data = path_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml")



result_model_bmi2c <- sem(path_model_bmi2c,
                         data = path_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml")



result_model_bmi2e <- sem(path_model_bmi2e,
                          data = path_df,
                          std.ov = FALSE,
                          cluster = "d_id_unim",
                          estimator = "mlr",
                          # group="chsex",
                          # Using FIML for "imputation"
                          missing = "fiml")

# SRQ ---------

result_model_srq1 <- sem(path_model_srq1,
                         data = path_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml")



result_model_srq2c <- sem(path_model_srq2c,
                          data = path_df,
                          std.ov = FALSE,
                          cluster = "d_id_unim",
                          estimator = "mlr",
                          # group="chsex",
                          # Using FIML for "imputation"
                          missing = "fiml")



result_model_srq2e <- sem(path_model_srq2e,
                          data = path_df,
                          std.ov = FALSE,
                          cluster = "d_id_unim",
                          estimator = "mlr",
                          # group="chsex",
                          # Using FIML for "imputation"
                          missing = "fiml")

# HAPPINESS ---------

result_model_happiness1 <- sem(path_model_happiness1,
                         data = path_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml")



result_model_happiness2c <- sem(path_model_happiness2c,
                          data = path_df,
                          std.ov = FALSE,
                          cluster = "d_id_unim",
                          estimator = "mlr",
                          # group="chsex",
                          # Using FIML for "imputation"
                          missing = "fiml")



result_model_happiness2e <- sem(path_model_happiness2e,
                          data = path_df,
                          std.ov = FALSE,
                          cluster = "d_id_unim",
                          estimator = "mlr",
                          # group="chsex",
                          # Using FIML for "imputation"
                          missing = "fiml")




# Summary ----------

all_models <- bind_rows(
  display_result_path(result_model_bmi1,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "BMI in 2016",
           mediator = "Direct"),
  display_result_path(result_model_bmi2c,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "BMI in 2016",
           mediator = "Community Respect"),
  display_result_path(result_model_bmi2e,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "BMI in 2016",
           mediator = "Economic Status"),
  
  display_result_path(result_model_srq1,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "SRQ-20 in 2018",
           mediator = "Direct"),
  display_result_path(result_model_srq2c,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "SRQ-20 in 2018",
           mediator = "Community Respect"),
  display_result_path(result_model_srq2e,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "SRQ-20 in 2018",
           mediator = "Economic Status"),
  
  display_result_path(result_model_happiness1,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "Happiness in 2018",
           mediator = "Direct"),
  display_result_path(result_model_happiness2c,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "Happiness in 2018",
           mediator = "Community Respect"),
  display_result_path(result_model_happiness2e,wider = FALSE,paper = "cohorts",display_type = "coef_ci") %>% 
    mutate(outcome = "Happiness in 2018",
           mediator = "Economic Status")
  
  
  ) 

all_models %>% 
  gt_rename_sss(.) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/path analysis summary.csv"),row.names = FALSE)

# Plot ----------

all_models %>% 
  dplyr::filter(iv %in% c(ses_vars,ladder_vars),dv %in% c("bmi","srq","happiness")) %>% 
  mutate(iv = case_when(iv %in% ladder_vars ~ "adladder",
                        TRUE ~ iv)) %>% 
  gt_rename_sss(.) %>% 
  mutate(iv = iv[order(match(iv,ses_vars))],
         mediator = factor(mediator,levels=c("Direct","Community Respect","Economic Status"),ordered=TRUE)) %>% 
  ggplot(data=.,aes(x=estimate,xmin=conf.low,xmax=conf.high,y=iv,group=mediator,col=mediator)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5)) +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)") +
  scale_color_manual(name="Model",values=c("black","red","green")) +
  theme(legend.position = "bottom") +
  facet_grid(~outcome,scales = "free_x")
  
  
all_models %>% 
  dplyr::filter(dv %in% c(ses_vars,ladder_vars),mediator == "Community Respect",outcome=="BMI in 2016") %>% 
  mutate(iv = case_when(iv %in% ladder_vars ~ "adladder",
                        TRUE ~ iv)) %>% 
  gt_rename_sss(.) %>%
  mutate(dv = dv[order(match(dv,ses_vars))]) %>% 
  ggplot(data=.,aes(x=estimate,xmin=conf.low,xmax=conf.high,y=iv,group=mediator,col=mediator)) +
  geom_point(position=position_dodge(width=0.5),col="green") +
  geom_errorbar(position=position_dodge(width=0.5),col="black") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)") +
  # theme(legend.position = "bottom") +
  facet_wrap(~dv,scales = "free_x",nrow = 2,ncol=5)

all_models %>% 
  dplyr::filter(dv %in% c(ses_vars,ladder_vars),mediator == "Economic Status",outcome=="BMI in 2016") %>% 
  mutate(iv = case_when(iv %in% ladder_vars ~ "adladder",
                        TRUE ~ iv)) %>% 
  gt_rename_sss(.) %>%
  mutate(dv = dv[order(match(dv,ses_vars))]) %>% 
  ggplot(data=.,aes(x=estimate,xmin=conf.low,xmax=conf.high,y=iv,group=mediator,col=mediator)) +
  geom_point(position=position_dodge(width=0.5),col="green") +
  geom_errorbar(position=position_dodge(width=0.5),col="black") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)") +
  # theme(legend.position = "bottom") +
  facet_wrap(~dv,scales = "free_x",nrow = 2,ncol=5)
