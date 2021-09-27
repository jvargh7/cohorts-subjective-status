
gt_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/multiple regression ipaw coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  separate(coef_ci,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(site = "Guatemala")

ph_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression ipaw coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  separate(coef_ci,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(site = "Philippines")

sa_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/multiple regression ipaw coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  separate(coef_ci,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(site = "South Africa")

sfig3_df <- bind_rows(gt_df,
                      ph_df,
                      sa_df) %>% 
  mutate(outcome = case_when(str_detect(variable,"BMI") ~ "BMI (kg/m2)",
                             str_detect(variable,"SRQ-20") ~ "WHO SRQ-20",
                             str_detect(variable,"Happiness") ~ "Subjective Happiness Scale",
                             TRUE ~ NA_character_
  ),
  sss = case_when(str_detect(model,"c$") ~ "Perceived Community Respect",
                  str_detect(model,"e$") ~ "Perceived Economic Status",
                  TRUE ~ NA_character_))

sfig3_df %>% 
  dplyr::filter(model %in% c("model3c","model3e")) %>% 
  
  ggplot(data=.,aes(x=coef,xmin=lci,xmax=uci,y=sss,col=site,shape=site)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),width=0) +
  facet_wrap(outcome~.,scales = "free_x") +
  geom_vline(xintercept=0,col="red",linetype=2) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8)) +
  scale_color_manual(name="",values=c("red","darkblue","green4")) +
  scale_shape_manual(name="",values=c(15:17)) +
  
  # scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) + 
  scale_y_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))


# Supplementary Table 3 -------


