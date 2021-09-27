
ph_rename_sss <- function(results_df){
  results_out <- results_df %>% 
    mutate_at(vars(dv,iv),
              function(x) case_when(
                x %in% c("adht","ht") ~ "Height in 2016",
                x %in% c("adbmi","bmi") ~ "BMI in 2016",
                x %in% c("adsrq","srq") ~ "SRQ-20 in 2018",
                x %in% c("adlifesat","lifesat") ~ "Life Satisfaction in 2018",
                x %in% c("adhappy","gtadhappytot2018","happiness") ~ "Happiness in 2018",
                x %in% c("adravenstotscore","ravens") ~ "Ravens in 2015-18",
                
                x == "pc1983" ~ "Wealth Index 1983",
                x == "pc1991" ~ "Wealth Index 1991",
                x == "pc1994" ~ "Wealth Index 1994",
                x == "pc1998" ~ "Wealth Index 1998",
                x == "pc2002" ~ "Wealth Index 2002",
                x == "pc2005" ~ "Wealth Index 2005",
                x == "pc2009" ~ "Wealth Index 2009",
                x == "pc2018" ~ "Wealth Index 2017-18",
                
                x %in% c("gtadladdercommunity2018","adladdercommunity") ~ "Community Ladder in 2017-18",
                x %in% c("gtadladdereconomic2018","adladdereconomic") ~ "Economic Ladder in 2017-18",
                
                x == "adladder" ~ "Subjective Social Status",
                
                x %in% c("eduyr","gtadeduyr1618") ~ "Attained schooling (y)",
                x == "gtatole" ~ "ATOLE VILLAGES",
                x == "chbirtho" ~ "Birth Order",
                x %in% c("rural","rural2016","rural2018","rural1","rural20161","rural20181") ~ "Rural resident",
                x %in% c("male","male1") ~ "Sex = Male",
                x %in% c("children","children1") ~ "Children = Yes"
                x %in% c("married","married1") ~ "Married = Yes"
                
                x == "moscho_imputed" ~ "Maternal schooling (imputed)",
                x == "moscho" ~ "Maternal schooling",
                x == "moage_imputed" ~ "Maternal age (imputed)",
                x == "moage" ~ "Maternal age",
                
                x == "score_change_ge300yes" ~ "(Holm Rahe Stress >= 300)",
                x %in% c("ademploymentformal","employment_formal","formal","formal1") ~ "Formal employment",
                
                x == "(Intercept)" ~ "INTERCEPT",
                TRUE ~ NA_character_
              ))
  
  return(results_out)
  
}