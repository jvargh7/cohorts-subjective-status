

retrieve_alive_df <- function(site = as.character(),
                              outcome_alive = "bmi"){

  outcome_alive = as.character(outcome_alive)
  if(site == "guatemala"){
    # gtml_non_responders(outcome_alive) %>% return(.)
    
    provided_sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/guatemala/provided_sss_df.RDS")) %>% 
        mutate(censoring_weight = case_when(outcome_alive == "bmi" ~ c_alive2016,
                                            outcome_alive %in% c("srq","ravens","happiness") ~ c_alive2018,
                                            TRUE ~ NA_real_)) %>% 
        dplyr::select(id_uni,censoring_weight,sss_weight)
      
    
  }
  if(site == "philippines"){
    # phil_non_responders(outcome_alive) %>% return(.)
    provided_sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/philippines/provided_sss_df.RDS")) %>% 
      dplyr::select(uncchdid,censoring_weight,sss_weight)
  }
  
  if(site == "south africa"){
    # phil_non_responders(outcome_alive) %>% return(.)
    provided_sss_df <- readRDS(paste0(path_dissertation,"/aim 3/working/cohorts/south africa/provided_sss_df.RDS")) %>% 
      dplyr::select(bttid,censoring_weight,sss_weight)
  }
  
  return(provided_sss_df)
  
}





