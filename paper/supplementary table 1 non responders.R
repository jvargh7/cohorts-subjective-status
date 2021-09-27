gt_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/alive_df.RDS")) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4)) %>% 
  left_join(readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/provided_sss_df.RDS")) %>% 
              dplyr::select(id_uni,provided_sss),
            by = "id_uni") %>% 
  mutate(lifestatus = case_when(provided_sss == 1 ~ 3,
                                provided_sss == 0 ~ 2,
                                alive2018 == 0 ~ 1,
                                TRUE ~ NA_real_))  %>% 
  dplyr::select(-moscho) %>% 
  rename(wealth_child = pcall6775_1,
         male = chsex,
         moscho = moscho_sib,
         id = id_uni)  %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,gtatole,lifestatus)

ph_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/alive_df.RDS")) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4)) %>% 
  left_join(readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/provided_sss_df.RDS")) %>% 
              dplyr::select(uncchdid,provided_sss),
            by = "uncchdid") %>% 
  mutate(lifestatus = case_when(provided_sss == 1 ~ 3,
                                provided_sss == 0 ~ 2,
                                alive2018 == 0 ~ 1,
                                TRUE ~ NA_real_),
         cstratum1983 = cstratum1983 - 1)  %>% 
  rename(wealth_child = pc1983,
         male = chsex,
         rural_child = cstratum1983,
         id = uncchdid)  %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,rural_child,lifestatus)
  

sa_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/alive_df.RDS")) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4),
         ethnicity = case_when(ethnicity == "black" ~ 1,
                               ethnicity %in% c("coloured","indian","white") ~ 0,
                               TRUE ~ NA_real_)) %>% 
  left_join(readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/provided_sss_df.RDS")) %>% 
              dplyr::select(bttid,provided_sss),
            by = "bttid") %>% 
  mutate(lifestatus = case_when(provided_sss == 1 ~ 3,
                                provided_sss == 0 ~ 2,
                                TRUE ~ NA_real_))  %>% 
  rename(wealth_child = pc1990,
         male = chsex,
         black = ethnicity,
         id = bttid)  %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,black,lifestatus)


stable1_df <- bind_rows(gt_df %>% 
                          mutate(site = "Guatemala",
                                 rural_child = rbinom(1,size = 1,prob=0.5),
                                 black = rbinom(1,size = 1,prob=0.5)),
                        ph_df %>% 
                          mutate(site = "Philippines",
                                 gtatole = rbinom(1,size = 1,prob=0.5),
                                 black = rbinom(1,size = 1,prob=0.5)),
                        sa_df %>% 
                          mutate(site = "South Africa",
                                 gtatole = rbinom(1,size = 1,prob=0.5),
                                 rural_child = rbinom(1,size = 1,prob=0.5))) %>% 
  mutate(lifestatus = factor(lifestatus,levels=c(1,2,3),labels=c("Died","Did not participate",
                                           "Provided SSS")))

# Compare Groups -------
library(compareGroups)

stable1_cg <- compareGroups(data=stable1_df,
                            # %>% dplyr::filter(site=="Philippines"),
                            site ~ moscho + moage + wealth_child +
                male + chbirtho + gtatole + rural_child +
                black,
              method = c(2,1,1,
                         3,2,3,3,
                         3),include.miss = TRUE,simplify = TRUE) %>% 
  createTable(.,digits=1,q.type = c(2,2),sd.type = 2,type = 1) 

strataTable(stable1_cg,strata="lifestatus") 


