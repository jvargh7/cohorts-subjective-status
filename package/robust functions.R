# For robust SE ----------
# https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r
# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf

# data=df
# formula_reg = formula1c_y
lm_robust <- function(formula_reg,site=NA,data=mids_df,clustering=FALSE){
  
  datlist <- miceadds::mids2datlist(data)
  
  if(clustering == FALSE){
    reg_out <- map(datlist,function(x) lm(formula_reg,x))
    
    
  }
  
  if(clustering == TRUE){
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

# Fraction of missing information:
# https://www.frontiersin.org/articles/10.3389/fpsyg.2021.667802/full
# The FMI measure quantifies the amount of information missing in the estimation of a 
# particular parameter (i.e., element of θ) by considering the drop in efficiency 
# when that parameter is estimated from the observed data Y rather than from the 
# hypothetical complete data X.

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
