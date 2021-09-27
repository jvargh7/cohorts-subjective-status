gt_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/multiple regression heterogeneity contrasts table.csv")) %>% 
  mutate(site = "Guatemala") %>% 
  dplyr::filter(!str_detect(model,"No Heterogeneity")) %>% 
  mutate(h = case_when(h == "pcall1618_1" ~ "pc2018",
                       TRUE ~ h)) %>% 
  mutate(ladder = case_when(str_detect(model,"Community Respect") ~ "Community Respect",
                            str_detect(model,"Economic Status") ~ "Economic Status",
                            TRUE ~ NA_character_)) %>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/multiple regression heterogeneity nested table.csv")) %>% 
              mutate(iv = "Contrast 1",
                     h = case_when(h == "pcall1618_1" ~ "pc2018",
                                   TRUE ~ h)),
            by = c("iv","y","h","ladder"="model"))

ph_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity contrasts table.csv")) %>% 
  mutate(site = "Philippines") %>% 
  dplyr::filter(!str_detect(model,"No Heterogeneity")) %>% 
  mutate(ladder = case_when(str_detect(model,"Community Respect") ~ "Community Respect",
                            str_detect(model,"Economic Status") ~ "Economic Status",
                            TRUE ~ NA_character_)) %>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/multiple regression heterogeneity nested table.csv")) %>% 
              mutate(iv = "Contrast 1"),
            by = c("iv","y","h","ladder"="model"))

sa_df <- read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/multiple regression heterogeneity contrasts table.csv")) %>% 
  mutate(site = "South Africa") %>% 
  dplyr::filter(!str_detect(model,"No Heterogeneity")) %>% 
  mutate(ladder = case_when(str_detect(model,"Community Respect") ~ "Community Respect",
                            str_detect(model,"Economic Status") ~ "Economic Status",
                            TRUE ~ NA_character_)) %>% 
  left_join(read_csv(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/multiple regression heterogeneity nested table.csv")) %>% 
              mutate(iv = "Contrast 1"),
            by = c("iv","y","h","ladder"="model"))

for (het_var in c("male","eduyrC","pc2018")){
  
  bind_rows(gt_df,
            ph_df,
            sa_df)  %>% 
    dplyr::filter(h == het_var) %>% 
    mutate(LRT = case_when(is.na(LRT_statistic) ~ "",
                           TRUE ~ paste0(round(LRT_statistic,2),", p = ",round(LRT_pvalue,3)))
           ) %>% 
    dplyr::select(-terms,-model,-LRT_statistic,-LRT_pvalue) %>% 
    pivot_wider(names_from = site,values_from=c("Coefficient","LRT"),values_fill = "") %>% 
    arrange(ladder,y,iv) %>% 
    dplyr::select(iv,y,h,ladder,matches("Guatemala"),matches("Philippines"),matches("South Africa")) %>% 
    write.csv(.,paste0(path_dissertation,"/aim 3/working/cohorts/supplementary table 2_",het_var,".csv"),row.names = FALSE)
  
}
