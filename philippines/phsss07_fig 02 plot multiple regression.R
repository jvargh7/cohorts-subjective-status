

result_path_mr <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression coefficients.csv")) %>% 
  pivot_longer(cols=contains("model"),names_to="model",values_to="coef_ci") %>% 
  separate(coef_ci,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.))

result_mr_contrasts <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity contrasts table.csv")) %>% 
  dplyr::filter(str_detect(model,"Heterogeneity:"),h == "male") %>% 
  mutate(model = case_when(str_detect(model,"Community Respect") ~ "model5c",
                           str_detect(model,"Economic Status") ~ "model5e",
                           TRUE ~ NA_character_))  %>% 
  separate(Coefficient,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(variable = case_when(y == "bmi" ~ "BMI in 2018",
                              y == "srq" ~ "SRQ-20 in 2018",
                              y == "happiness" ~ "Happiness in 2018",
                              TRUE ~ NA_character_)) %>% 
  mutate(model = case_when(iv == "Contrast 1" ~ paste0(model," female"),
                           iv == "Contrast 2" ~ paste0(model," male"),
                           iv == "Contrast 3" ~ paste0(model," Male - Female"),
                           TRUE ~ NA_character_)) %>% 
  dplyr::select(model,variable,coef,lci,uci)


# PLOT ----------
bind_rows(result_path_mr,
          result_mr_contrasts) %>% 
  mutate(
    sss = case_when(str_detect(model,"[0-9](A|B|C)*c") ~ "Community Respect",
                    str_detect(model,"[0-9](A|B|C)*e") ~ "Economic Status",
                    TRUE ~ NA_character_
    )) %>% 
  mutate(model = str_to_title(model)) %>% 
  ggplot(data=.,aes(x=coef,y=model,xmin=lci,xmax=uci)) +
  geom_point() +
  geom_errorbar() +
  facet_grid(sss~variable,scales="free") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)")



