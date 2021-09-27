# Using mice package ----------

mice_coefs_heterogeneity <- function(summary_mice,
                                     model = ""){
  
  summary_mice %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error,
           model = model) %>% 
    mutate(Coefficient = paste0(
      estimate %>% round(.,2)," (",
      lci %>% round(.,2),", ",
      uci %>% round(.,2),")"
    )) %>% 
    return(.)
  
}

mice_heterogeneity <- function(o,miaux_dfs,het_var="",site="guatemala"){
  
  adult_life_x <- c("lifesat + children")  # + married
  
  rural = ""
  if(site == "guatemala"){
    wealth_var = "pcall1618_1"
    sep_x <- paste0("+ pcall6775_1 + pcall1618_1 + eduyrC + formal")
    early_life_x <- "moscho_sib + moage + male + chbirtho + gtatole + full + gtatole:full + byear"
    
    if(o == "bmi"){rural = " + rural2016"}
    if(o %in% c("srq","happiness")){rural = " + rural2018"}
    
    
  }
  if(site == "philippines"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1983 + pc2018 + eduyrC + formal"
    rural = " + rural2018"
    early_life_x <- "moscho + moage + male + chbirtho + rural1983"
    
  }
  
  if(site == "south africa"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1990 + pc2018 + eduyrC + formal"
    rural = ""
    early_life_x <- "moscho + moage + male + chbirtho + black"
    
  }
  
  adult_life_x = paste0(adult_life_x,rural)
  
  formula3c_y <- paste0(o,"~ adladdercommunity +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula3e_y <- paste0(o,"~ adladdereconomic +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula5c_y <- paste0(o,"~ adladdercommunity + adladdercommunity:",het_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula5e_y <- paste0(o,"~ adladdereconomic + adladdereconomic:",het_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  
  models_list_3c <- lm.mids(formula3c_y,data=miaux_dfs) 
  models_list_3e <- lm.mids(formula3e_y,data=miaux_dfs)
  models_list_5c <- lm.mids(formula5c_y,data=miaux_dfs) 
  models_list_5e <- lm.mids(formula5e_y,data=miaux_dfs)
  
  summary_table <- bind_rows(models_list_3c %>% pool(.) %>% summary(.)%>% 
                                mice_coefs_heterogeneity(.,model="Community Respect, No Heterogeneity"),
                              models_list_5c %>% pool(.) %>% summary(.)%>% 
                                mice_coefs_heterogeneity(.,model=paste0("Community Respect, Heterogeneity: ",het_var)),
                              models_list_3e %>% pool(.) %>% summary(.)%>% 
                                mice_coefs_heterogeneity(.,model="Economic Respect, No Heterogeneity"),
                              models_list_5e %>% pool(.) %>% summary(.)%>% 
                                mice_coefs_heterogeneity(.,model=paste0("Community Respect, Heterogeneity: ",het_var))
                              )
  
  anova_c <- D3(models_list_5c,models_list_3c)$result
  anova_e <- D3(models_list_5e,models_list_3e)$result
  model_comparison <- bind_rows(data.frame(model = "Community Respect",
                                           LRT_statistic = anova_c[1],
                                           LRT_pvalue = anova_c[4]),
                                  
                                data.frame(model = "Economic Status",
                                           LRT_statistic = anova_e[1],
                                           LRT_pvalue = anova_e[4]))
  return(list(summary_table,
              model_comparison))
  
  
  
}



# Manual replication -----------
consolidated_heterogeneity <- function(o,miaux_dfs,het_var="",site="guatemala"){
  
  adult_life_x <- c("lifesat + children")  # + married
  
  rural = ""
  if(site == "guatemala"){
    wealth_var = "pcall1618_1"
    sep_x <- paste0("+ pcall6775_1 + pcall1618_1 + eduyrC + formal")
    early_life_x <- "moscho_sib + moage + male + chbirtho + gtatole + full + gtatole:full + byear"
    
    if(o == "bmi"){rural = " + rural2016"}
    if(o %in% c("srq","happiness")){rural = " + rural2018"}
    
    
  }
  if(site == "philippines"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1983 + pc2018 + eduyrC + formal"
    rural = " + rural2018"
    early_life_x <- "moscho + moage + male + chbirtho + rural1983"
    
  }
  
  if(site == "south africa"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1990 + pc2018 + eduyrC + formal"
    rural = ""
    early_life_x <- "moscho + moage + male + chbirtho + black"
    
  }
  
  adult_life_x = paste0(adult_life_x,rural)
  
  formula3c_y <- paste0(o,"~ adladdercommunity +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula3e_y <- paste0(o,"~ adladdereconomic +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula5c_y <- paste0(o,"~ adladdercommunity + adladdercommunity:",het_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula5e_y <- paste0(o,"~ adladdereconomic + adladdereconomic:",het_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  
  models_list_3c <- list()
  models_list_3e <- list()
  models_list_5c <- list()
  models_list_5e <- list()
  
  for (i in 1:miaux_dfs$m){
    
    df = complete(miaux_dfs,i)
    if(y == "bmi"){
      df <- df %>% 
        mutate(bmi = case_when(pregnant == 1 ~ NA_real_,
                               TRUE ~ bmi))
    }
    glm_3c <- lm(formula3c_y,data = df)
    glm_5c <- lm(formula5c_y,data = df)
    glm_3e <- lm(formula3e_y,data = df)
    glm_5e <- lm(formula5e_y,data = df)
    
    # glm_3c <- geepack::geeglm(formula3c_y,
    #                           id = d_id_unim,
    #                           corstr = "unstructured",
    #                           data = df)

    
    # glm_5c <- geepack::geeglm(formula5c_y,
    #                           id = d_id_unim,
    #                           corstr = "unstructured",
    #                           data = df)
    
    
    # glm_3e <- geepack::geeglm(formula3e_y,
    #                           id = d_id_unim,
    #                           corstr = "unstructured",
    #                           data = df)
    # 
    # glm_5e <- geepack::geeglm(formula5e_y,
    #                           id = d_id_unim,
    #                           corstr = "unstructured",
    #                           data = df)
    models_list_3c[[i]] <- glm_3c
    models_list_5c[[i]] <- glm_5c
    models_list_3e[[i]] <- glm_3e
    models_list_5e[[i]] <- glm_5e
  }
  
  all_models <- list(models_list_3c = models_list_3c,
                     models_list_5c = models_list_5c,
                     models_list_3e = models_list_3e,
                     models_list_5e = models_list_5e
  )
  
  vec_het_var = complete(miaux_dfs,i) %>% 
    dplyr::select(one_of(het_var)) %>% 
    pull()
  
  het_var_coefname = het_var
  if(class(vec_het_var) == "factor"){
    het_var_coefname = paste0(het_var,"1")
    
  }
  summary_table <- coefs_heterogeneity(all_models,het_var = het_var_coefname)
  
  
  contrast_matrices <- prepare_contrasts(glm_3c,glm_5c,
                                         het_var = het_var_coefname,
                                         exposure = c("adladdercommunity","adladdereconomic"))
  contrasts_table <- contrasts_summary(all_models,
                                       contrast_matrices,het_var=het_var_coefname)
  
  return(list(summary_table,
              contrasts_table))
  
}


coefs_heterogeneity <- function(model_list,het_var){
  
  models_list_3c = model_list[["models_list_3c"]]
  models_list_5c = model_list[["models_list_5c"]]
  models_list_3e = model_list[["models_list_3e"]]
  models_list_5e = model_list[["models_list_5e"]]
  
  
  coefs_table <- bind_rows(clean_mi_conditionalregression(models_list_3c,link = "lmer identity") %>% 
                             # dplyr::select(iv, Coefficient) %>% 
                             mutate(model = "Community Respect, No Heterogeneity"),
                           
                           clean_mi_conditionalregression(models_list_5c,link = "lmer identity") %>% 
                             # dplyr::select(iv, Coefficient)  %>% 
                             mutate(model = paste0("Community Respect, Heterogeneity: ",het_var)),
                           
                           clean_mi_conditionalregression(models_list_3e,link = "lmer identity") %>% 
                             # dplyr::select(iv, Coefficient) %>% 
                             mutate(model = "Economic Status, No Heterogeneity"),
                           
                           clean_mi_conditionalregression(models_list_5e,link = "lmer identity") %>% 
                             # dplyr::select(iv, Coefficient)  %>% 
                             mutate(model = paste0("Economic Status, Heterogeneity: ",het_var)),
                           
  )
  
  return(coefs_table)

}


prepare_contrasts <- function(glm_nohet_example,glm_het_example,het_var,exposure){
  
  # glm_nohet_example <- glm_3c
  # glm_het_example <- glm_5c
  # het_var <- "male"
  # exposure <- c("adladdercommunity","adladdereconomic")
  
  
  nterms_nohet <- length(glm_nohet_example$coefficients)
  nterms_het <- length(glm_het_example$coefficients)
  
  names_nohet <- attr(glm_nohet_example$coefficients,"names")
  names_het <- attr(glm_het_example$coefficients,"names")
  
  contrast_matrix_nohet = matrix(c(
    rep(c(0),each=nterms_nohet),
    rep(c(0),each=nterms_nohet)
  ),
  nrow=2,
  byrow=TRUE
  )
  
  # Het Var = 1: Fixed effect
  contrast_matrix_nohet[1,which(names_nohet %in% het_var)] <- 1
  
  # Exposure: Fixed effect
  contrast_matrix_nohet[2,which(names_nohet %in% exposure)] <- 1
  
  
  contrast_matrix_het = matrix(c(
    rep(c(0),each=nterms_het),
    rep(c(0),each=nterms_het),
    rep(c(0),each=nterms_het)
  ),
  nrow=3,
  byrow=TRUE
  )
  
  # Exposure in Het Var = 0
  contrast_matrix_het[1,which(names_het %in% exposure)] <- 1
  
  # Exposure in Het Var = 1
  contrast_matrix_het[2,which(names_het %in% c(exposure,paste0(exposure,":",het_var)))] <- 1
  
  # Interaction effect of Exposure x Het Var
  contrast_matrix_het[3,which(names_het %in% c(paste0(exposure,":",het_var)))] <- 1
  
  return(list(contrast_matrix_nohet,
              contrast_matrix_het))
  
}


contrasts_summary <- function(model_list,contrast_matrices,het_var){
  
  models_list_3c = model_list[["models_list_3c"]]
  models_list_5c = model_list[["models_list_5c"]]
  models_list_3e = model_list[["models_list_3e"]]
  models_list_5e = model_list[["models_list_5e"]]
  
  contrasts_summary_table <- bind_rows(
    clean_mi_contrasts(models_list_3c,model_matrix = contrast_matrices[[1]],vcov_type="robust") %>% 
      dplyr::select(iv, Coefficient) %>% 
      mutate(model = "Community Respect, No Heterogeneity",
             terms = c(paste0("FE: ",het_var),
                       "Community Respect"))
      ,
    clean_mi_contrasts(models_list_5c,model_matrix = contrast_matrices[[2]],vcov_type="robust") %>% 
      dplyr::select(iv, Coefficient)  %>% 
      mutate(model = paste0("Community Respect, Heterogeneity: ",het_var),
             terms = c(paste0("Community Respect at ",het_var," = 0"),
                       paste0("Community Respect at ",het_var," = 1"),
                       "Interaction")),
    
    clean_mi_contrasts(models_list_3e,model_matrix = contrast_matrices[[1]],vcov_type="robust") %>% 
      dplyr::select(iv, Coefficient) %>% 
      mutate(model = "Economic Status, No Heterogeneity",
             terms = c(paste0("FE: ",het_var),
                       "Economic Status")),
    
    clean_mi_contrasts(models_list_5e,model_matrix = contrast_matrices[[2]],vcov_type="robust")%>% 
      dplyr::select(iv, Coefficient)  %>% 
      mutate(model = paste0("Economic Status, Heterogeneity: ",het_var),
             terms = c(paste0("Economic Status at ",het_var," = 0"),
                       paste0("Economic Status at ",het_var," = 1"),
                       "Interaction"))
  )
  
  return(contrasts_summary_table)
  
}

