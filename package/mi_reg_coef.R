# Manual replication -------------
source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_pathanalysis.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))

library(mice)

# formula_reg = formula3c_y
# i = 4
mi_reg_coef <- function(formula_reg,mi_dfs,
                        site = as.character(),
                        coef_name = as.character(),
                        coef_type = "Coefficient",
                        ipaw = FALSE){
  
  models_list <- list()
  
  id_var = case_when(site == "brazil 1993" ~ "nquest",
                     site == "guatemala" ~ "id_uni",
                     site == "philippines" ~ "uncchdid",
                     site == "south africa" ~ "bttid",
                     site == "india" ~ "id",
                     TRUE ~ NA_character_)
  
  if(!id_var %in% names(mi_dfs$data)){
    print("ID variable missing")
  }
  
  for (i in 1:mi_dfs$m){
    
    df = complete(mi_dfs,i)
    
    
    o = str_split(formula_reg,pattern=" ~ ",n=2)[[2]]
    rhs_formula_o = str_split(formula_reg,pattern=" ~ ",n=2)[[3]]
    df$na_outcome = is.na(mi_dfs$data[,o])  %>% as.numeric(.)
    
    if(o == "bmi"){
       df <- df %>% 
        mutate(
               bmi = case_when(pregnant == 1 ~ NA_real_,
                               TRUE ~ bmi))
    }
    
    # IPAW = FALSE ------
    if(ipaw == FALSE){
      # glm_c <- lm(formula_reg, data = df)
      
      glm_c <- MASS::rlm(formula_reg, data = df)
      
    }
    
    # IPAW = TRUE -------
    if(ipaw == TRUE){
     df[,o] <- mi_dfs$data[,o] 
    # rhs_formula_a (formula to predict being alive) is set for each site separately
    # Using retrieve_alive_df for cleaner code instead of many IF statements 
     df_alive = retrieve_alive_df(site = site,outcome_alive = o) 
     
     r_w = 1
     if(mean(df$na_outcome) > 0.05){
       r_w = censoring_weights(c_formula = paste0("na_outcome ~ ", 
                                                  rhs_formula_o),
                               df = df,
                               type = "glm")
     }
     
    
      
      df <- df %>%
        left_join(df_alive,
                  by = id_var) %>% 
        mutate(response_weight = r_w) %>% 
        mutate('id' = id_var) %>% 
        mutate(weight = censoring_weight*response_weight*sss_weight)

      # print(i)
      glm_c <- MASS::rlm(formula_reg, data = df,
                 weights = weight,wt.method="case")
      
      # glm_c <- geeglm(formula_reg, data = df,
      #                 weights = weight,
      #                 id = id,
      #                 corstr = "independence")
    }
    
    models_list[[i]] <- glm_c
  }
  
  # coef_type = coef_type
  # print(coef_type)
  out_coef = extract_mi_coef_type(models_list = models_list,
                                  coef_name = coef_name,
                                  coef_type = coef_type)
  
  return(list(out_coef,
              models_list))
  
}

temp_mr <- function(o,dfs,type="mi",
                    coef_type = "Coefficient",
                    site = "guatemala",
                    ipaw = FALSE){
  
  adult_life_x <- c("lifesat + children + married")  # 
  
  rural = ""
  if(site == "guatemala"){
    wealth_var = "pcall1618_1"
    sep_x <- paste0("+ pcall6775_1 + pcall1618_1 + eduyr + formal")
    early_life_x <- "moscho_sib + moage + male + chbirtho + gtatole + full + gtatole:full + byear"
    
    if(o == "bmi"){rural = " + rural2016"}
    if(o %in% c("srq","happiness")){rural = " + rural2018"}
    
    
  }
  if(site == "philippines"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1983 + pc2018 + eduyr + formal"
    rural = " + rural2018"
    early_life_x <- "moscho + moage + male + chbirtho + rural1983"
   
  }
  
  if(site == "south africa"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1990 + pc2018 + eduyr + formal"
    rural = ""
    early_life_x <- "moscho + moage + male + chbirtho + black"
    
  }
  
  adult_life_x = paste0(adult_life_x,rural)
  
  
  
  formula1c_y <- paste0(o,"~ adladdercommunity") %>% as.formula()
  formula2c_y <- paste0(o,"~ adladdercommunity +",sep_x)%>% as.formula()
  formula3c_y <- paste0(o,"~ adladdercommunity +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ac_y <- paste0(o,"~ adladdercommunity*male +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Bc_y <- paste0(o,"~ adladdercommunity*eduyr +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Cc_y <- paste0(o,"~ adladdercommunity*",wealth_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  
  formula1e_y <- paste0(o,"~ adladdereconomic") %>% as.formula()
  formula2e_y <- paste0(o,"~ adladdereconomic +",sep_x)%>% as.formula()
  formula3e_y <- paste0(o,"~ adladdereconomic +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ae_y <- paste0(o,"~ adladdereconomic*male +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Be_y <- paste0(o,"~ adladdereconomic*eduyr +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ce_y <- paste0(o,"~ adladdereconomic*",wealth_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  if(type == "mi"){
    
    model1c_out = mi_reg_coef(formula1c_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    model2c_out = mi_reg_coef(formula2c_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    model3c_out = mi_reg_coef(formula3c_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    model4Ac_out = mi_reg_coef(formula4Ac_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    model4Bc_out = mi_reg_coef(formula4Bc_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    model4Cc_out = mi_reg_coef(formula4Cc_y,site=site,mi_dfs = dfs,coef_name = "adladdercommunity",ipaw=ipaw)
    
    model1e_out = mi_reg_coef(formula1e_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)
    model2e_out = mi_reg_coef(formula2e_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)
    model3e_out = mi_reg_coef(formula3e_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)
    model4Ae_out = mi_reg_coef(formula4Ae_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)
    model4Be_out = mi_reg_coef(formula4Be_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)
    model4Ce_out = mi_reg_coef(formula4Ce_y,site=site,mi_dfs = dfs,coef_name = "adladdereconomic",ipaw=ipaw)

    mr_df <- data.frame(variable = o,
                        model1c = model1c_out[[1]],
                        model2c = model2c_out[[1]],
                        model3c = model3c_out[[1]],
                        model4Ac = model4Ac_out[[1]],
                        model4Bc = model4Bc_out[[1]],
                        model4Cc = model4Cc_out[[1]],
                        model1e = model1e_out[[1]],
                        model2e = model2e_out[[1]],
                        model3e = model3e_out[[1]],
                        model4Ae = model4Ae_out[[1]],
                        model4Be = model4Be_out[[1]],
                        model4Ce = model4Ce_out[[1]]
    )
    
    model_list <- list(
      model1c = model1c_out[[2]],
      model2c = model2c_out[[2]],
      model3c = model3c_out[[2]],
      model4Ac = model4Ac_out[[2]],
      model4Bc = model4Bc_out[[2]],
      model4Cc = model4Cc_out[[2]],
      model1e = model1e_out[[2]],
      model2e = model2e_out[[2]],
      model3e = model3e_out[[2]],
      model4Ae = model4Ae_out[[2]],
      model4Be = model4Be_out[[2]],
      model4Ce = model4Ce_out[[2]]
      
    )
    
  }
  

  return(list(mr_df,model_list))
  
  
  
}


source(paste0(path_sss_repo,"/package/robust functions.R"))

# Using mice package ---------

mice_reg_coef <- function(summary_pooled,
                          coef_name = "adladdercommunity",type="lm"){
  if(type == "lm"){
    summary_pooled %>% 
      dplyr::filter(term == coef_name) %>% 
      mutate(lci = estimate - 1.96*std.error,
             uci = estimate + 1.96*std.error) %>% 
      mutate(Coefficient = paste0(
        estimate %>% round(.,2)," (",
        lci %>% round(.,2),", ",
        uci %>% round(.,2),")"
      )) %>% 
      dplyr::select(Coefficient) %>% 
      pull(.) %>% 
      return(.)
  }

  if(type == "lm_robust"){
    summary_pooled %>% 
      dplyr::filter(term==coef_name) %>% 
      mutate(Coefficient = paste0(
        estimate %>% round(.,2)," (",
        lci %>% round(.,2),", ",
        uci %>% round(.,2),")"
      )) %>% 
      dplyr::select(Coefficient) %>% 
      pull(.) %>% 
      return(.)
  }
  
}

# o = y


mice_pool_reg <- function(o,df,type="robust mi",coef_type = "Coefficient",
                          site = "guatemala",
                          scaled = FALSE,
                          all_wealth = FALSE,
                          output_type = "mr_df"){
  adult_life_x <- c("lifesat + children + married")  # Only in the COHORTS paper
  
  rural = ""
  if(site == "guatemala"){
    wealth_var = "pcall1618_1"
    sep_x <- paste0("+ pcall6775_1 + pcall1618_1 + eduyr + formal")
    early_life_x <- "moscho_sib + moage + male + chbirtho + gtatole + byear"
    
    if(o == "bmi"){rural = " + rural2016"}
    if(o %in% c("srq","happiness")){rural = " + rural2018"}
    if(all_wealth){sep_x = paste0(sep_x," + pcall1987_1 + pcall1996_1 + pcall2002_1")}
    
  }
  if(site == "philippines"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1983 + pc2018 + eduyr + formal"
    rural = " + rural2018"
    early_life_x <- "moscho + moage + male + chbirtho + rural1983"
    if(all_wealth){sep_x = paste0(sep_x," + pc1991 + pc1994 + pc1998 + pc2002 + pc2005 + pc2009")}
    
  }
  
  if(site == "south africa"){
    wealth_var = "pc2018"
    sep_x <- "+ pc1990 + pc2018 + eduyr + formal"
    rural = ""
    early_life_x <- "moscho + moage + male + chbirtho + black"
    if(all_wealth){sep_x = paste0(sep_x," + pc1997 + pc2002 + pc2006 + pc2012")}
    
  }
  
  adult_life_x = paste0(adult_life_x,rural)
  
  variable = o
  community_var = "adladdercommunity"
  economic_var = "adladdereconomic"
  if(scaled == TRUE){
    # o = paste0("scale(",o,")")
    community_var = "scale(adladdercommunity)"
    economic_var = "scale(adladdereconomic)"
    
  }
  
  
  formula1c_y <- paste0(o,"~ ",community_var) %>% as.formula()
  formula2c_y <- paste0(o,"~ ",community_var," +",sep_x)%>% as.formula()
  formula3c_y <- paste0(o,"~ ",community_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ac_y <- paste0(o,"~ ",community_var,"*male +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Bc_y <- paste0(o,"~ ",community_var,"*eduyr +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Cc_y <- paste0(o,"~ ",community_var,"*",wealth_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  # From SSMPH 2021 paper
  # formula3c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + 
  #                       gtatole + exposure1000 + atole1000 + 
  #                       moage_imputed + byear + sexM + 
  #                       pcall6775_1 + pcall1618_1 + 
  #                       gtadeduyr1618 + rural + employmentyn")
  
  formula1e_y <- paste0(o,"~ ",economic_var) %>% as.formula()
  formula2e_y <- paste0(o,"~ ",economic_var," +",sep_x)%>% as.formula()
  formula3e_y <- paste0(o,"~ ",economic_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ae_y <- paste0(o,"~ ",economic_var,"*male +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Be_y <- paste0(o,"~ ",economic_var,"*eduyr +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  formula4Ce_y <- paste0(o,"~ ",economic_var,"*",wealth_var," +",sep_x," +",early_life_x," +",adult_life_x) %>% as.formula()
  
  if(type == "mi"){
    
    model1c_out = lm.mids(formula1c_y,data=df) %>% pool(.) %>% summary(.)
    model2c_out = lm.mids(formula2c_y,data=df) %>% pool(.) %>% summary(.)
    model3c_out = lm.mids(formula3c_y,data=df) %>% pool(.) %>% summary(.)
    model4Ac_out = lm.mids(formula4Ac_y,data=df) %>% pool(.) %>% summary(.)
    model4Bc_out = lm.mids(formula4Bc_y,data=df) %>% pool(.) %>% summary(.)
    model4Cc_out = lm.mids(formula4Cc_y,data=df) %>% pool(.) %>% summary(.)
    
    model1e_out = lm.mids(formula1e_y,data=df) %>% pool(.) %>% summary(.)
    model2e_out = lm.mids(formula2e_y,data=df) %>% pool(.) %>% summary(.)
    model3e_out = lm.mids(formula3e_y,data=df) %>% pool(.) %>% summary(.)
    model4Ae_out = lm.mids(formula4Ae_y,data=df) %>% pool(.) %>% summary(.)
    model4Be_out = lm.mids(formula4Be_y,data=df) %>% pool(.) %>% summary(.)
    model4Ce_out = lm.mids(formula4Ce_y,data=df) %>% pool(.) %>% summary(.)
    
    mr_df <- data.frame(variable = variable,
                        model1c = model1c_out %>% mice_reg_coef(.,coef_name=community_var),
                        model2c = model2c_out %>% mice_reg_coef(.,coef_name=community_var),
                        model3c = model3c_out %>% mice_reg_coef(.,coef_name=community_var),
                        model4Ac = model4Ac_out %>% mice_reg_coef(.,coef_name=community_var),
                        model4Bc = model4Bc_out %>% mice_reg_coef(.,coef_name=community_var),
                        model4Cc = model4Cc_out %>% mice_reg_coef(.,coef_name=community_var),
                        model1e = model1e_out %>% mice_reg_coef(.,coef_name=economic_var),
                        model2e = model2e_out %>% mice_reg_coef(.,coef_name=economic_var),
                        model3e = model3e_out %>% mice_reg_coef(.,coef_name=economic_var),
                        model4Ae = model4Ae_out %>% mice_reg_coef(.,coef_name=economic_var),
                        model4Be = model4Be_out %>% mice_reg_coef(.,coef_name=economic_var),
                        model4Ce = model4Ce_out %>% mice_reg_coef(.,coef_name=economic_var)
    )
    
    

  }
  
  
  
  if(type == "robust mi"){
    
    model1c_out = lm_robust(formula1c_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model2c_out = lm_robust(formula2c_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model3c_out = lm_robust(formula3c_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    model4Ac_out = lm_robust(formula4Ac_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model4Bc_out = lm_robust(formula4Bc_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model4Cc_out = lm_robust(formula4Cc_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    
    model1e_out = lm_robust(formula1e_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    model2e_out = lm_robust(formula2e_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model3e_out = lm_robust(formula3e_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    model4Ae_out = lm_robust(formula4Ae_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    model4Be_out = lm_robust(formula4Be_y,site,data=df) %>% pool_robust(.)  %>% summary_robust(.)
    model4Ce_out = lm_robust(formula4Ce_y,site,data=df)  %>% pool_robust(.) %>% summary_robust(.)
    
    mr_df <- data.frame(variable = variable,
                        model1c = model1c_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model2c = model2c_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model3c = model3c_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model4Ac = model4Ac_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model4Bc = model4Bc_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model4Cc = model4Cc_out %>% mice_reg_coef(.,coef_name=community_var,type="lm_robust"),
                        model1e = model1e_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust"),
                        model2e = model2e_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust"),
                        model3e = model3e_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust"),
                        model4Ae = model4Ae_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust"),
                        model4Be = model4Be_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust"),
                        model4Ce = model4Ce_out %>% mice_reg_coef(.,coef_name=economic_var,type="lm_robust")
    )

    

    
  }
  
  all_model_coefs = bind_rows(
    model1c = model1c_out %>% mutate(model = "model1c"),
    model2c = model2c_out %>% mutate(model = "model2c"),
    model3c = model3c_out  %>% mutate(model = "model3c"),
    model4Ac = model4Ac_out %>% mutate(model = "model4Ac"),
    model4Bc = model4Bc_out %>% mutate(model = "model4Bc"),
    model4Cc = model4Cc_out %>% mutate(model = "model4Cc"),
    model1e = model1e_out  %>% mutate(model = "model1e"),
    model2e = model2e_out %>% mutate(model = "model2e"),
    model3e = model3e_out  %>% mutate(model = "model3e"),
    model4Ae = model4Ae_out %>% mutate(model = "model4Ae"),
    model4Be = model4Be_out  %>% mutate(model = "model4Be"),
    model4Ce = model4Ce_out %>% mutate(model = "model4Ce")
  )
  
  
  
  
  if(output_type == "mr_df"){
    return(mr_df)
  }
  if(output_type == "all_model_coefs"){
    return(list(mr_df,all_model_coefs))
  }
  
  
  
}


