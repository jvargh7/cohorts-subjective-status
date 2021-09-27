
gt_rename_sss <- function(results_df){
  results_out <- results_df %>% 
    mutate_at(vars(dv,iv),
              function(x) case_when(
                x %in% c("adht","ht") ~ "Height in 2016",
                x %in% c("adbmi","bmi") ~ "BMI in 2016",
                x %in% c("adsrq","srq") ~ "SRQ-20 in 2018",
                x %in% c("adlifesat","lifesat") ~ "Life Satisfaction in 2018",
                x %in% c("adhappy","gtadhappytot2018","happiness") ~ "Happiness in 2018",
                x %in% c("adravenstotscore","ravens") ~ "Ravens in 2015-18",
                
                x == "pcall6775_1" ~ "Wealth Index 1967-75",
                x == "pcall1987_1" ~ "Wealth Index 1987",
                x == "pcall1996_1" ~ "Wealth Index 1996",
                x == "pcall2002_1" ~ "Wealth Index 2002",
                x == "pcall2016_1" ~ "Wealth Index 2016",
                x == "pcall2018_1" ~ "Wealth Index 2018",
                x == "pcall1618_1" ~ "Wealth Index 2015-18",
                
                x %in% c("gtadladdercommunity2018","adladdercommunity") ~ "Community Ladder in 2017-18",
                x %in% c("gtadladdereconomic2018","adladdereconomic") ~ "Economic Ladder in 2017-18",
                
                x == "adladder" ~ "Subjective Social Status",
                
                x %in% c("eduyr","gtadeduyr1618") ~ "Attained schooling (y)",
                x == "gtatole" ~ "Atole villages",
                x == "chbirtho" ~ "Birth order",
                x == "chbirtho_imputed" ~ "Birth order (Imputed)",
                x %in% c("rural","rural2016","rural2018","rural1","rural20161","rural20181") ~ "Rural resident",
                x == "byear" ~ "Birth Year - 1970",
                x == "full" ~ "Period = Full",
                x == "atolefull" ~ "Atole x Full",
                x %in% c("male","male1") ~ "Sex = Male",
                
                x %in% c("children","children1") ~ "Children = Yes"
                x %in% c("married","married1") ~ "Married = Yes"
                
                x == "moscho_imputed" ~ "Maternal schooling (imputed)",
                x == "moage_imputed" ~ "Maternal age (imputed)",
                
                x == "score_change_ge300yes" ~ "(Holm Rahe Stress >= 300)",
                x %in% c("ademploymentformal","employment_formal","formal","formal1") ~ "Formal employment",
                
                x == "(Intercept)" ~ "INTERCEPT",
                TRUE ~ NA_character_
              ))
  
  return(results_out)
  
}