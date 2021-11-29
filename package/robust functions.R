# For robust SE ----------
# https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r
# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf

# data=df
# formula_reg = formula1c_y
lm_robust <- function(formula_reg,site=NA,data=mids_df,clustering=TRUE){
  
  datlist <- miceadds::mids2datlist(data)
  print(site)
  if(clustering == FALSE|site == "south africa"){
    reg_out <- map(datlist,function(x) lm(formula_reg,x))
  }
  
  if(clustering == TRUE & site != "south africa"){
    print("running GEE")
    require(geepack)
    if(site == "guatemala"){
      cluster_id = "d_id_unim"
    }
    if(site == "philippines"){
      cluster_id = "currbrgy"
    }
    reg_out <- map(datlist,function(x) {
      df_x = x;
      
      df_x$clusters = x %>% dplyr::select(cluster_id) %>% pull();
      
      geeglm(formula_reg,corstr = "exchangeable",data = df_x,
             id = clusters)
    })
  }
  
  
  
  
  
  return(reg_out)
  
}


pool_robust <- function(reg_out){
  
  betas <- map(reg_out,coef)
  vars <- map(reg_out,function(lm_x) {
    # print(attr(lm_x,"class")[1])
    if(attr(lm_x,"class")[1] == "lm"){
      W = sandwich::vcovHC(lm_x, "HC0")
    };

    if(attr(lm_x,"class")[1] == "geeglm"){
      W = lm_x$geese$vbeta
    };
    
    return(W)
  })
              
  model_out <- miceadds::pool_mi(betas,vars) 
  
  return(model_out)
  
}


# lm_out = model_out
summary_robust <- function(lm_out){
  
  summary_out <- summary(lm_out) %>% 
    mutate(term = rownames(.)) %>% 
    rename(estimate = results,
           std.error = se)  %>% 
    rename(
      lci = '(lower',
      uci = 'upper)')
  rownames(summary_out) <- NULL
  
  summary_out %>% 
    return(.)
}

gof_robust <- function(models_large,models_small,models_type){
  
  if(models_type=="mi"){
    LRT = D3(models_large,models_small)$result;
  }
  
  if(models_type == "robust mi"){
    LRT_models = map2(models_large,models_small,
               function(x,y){
                 QIC_large = QIC(x);
                 QIC_small = QIC(y);
                 
                 chisq_diff = 2*(QIC_large[["Quasi Lik"]] - QIC_small[["Quasi Lik"]]);
                 p_val = 1-pchisq(chisq_diff,1)
                 # print(paste0("P = ",p_val))
                 return(c(chisq_diff,p_val))
                 
                })
    LRT = numeric()
    LRT[1] = map(LRT_models,
                 function(x) x[[1]][1]) %>% 
      unlist() %>% 
      mean(.,na.rm=TRUE)
    
    LRT[2] = NA
    LRT[3] = NA
    LRT[4] = map(LRT_models,
                 function(x) x[2]) %>% 
      unlist() %>% 
      as.numeric(.) %>% 
      mean(.,na.rm=TRUE)
    
    # print(paste0("LRT = ",LRT[4]))
    
  }
  
  return(LRT)
  
  
}
