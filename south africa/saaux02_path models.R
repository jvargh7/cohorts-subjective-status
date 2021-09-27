
# Variables ---------

early_life_vars <- c("moage_imputed","chbirtho","male","black")
ses_vars <- c("moscho_imputed","pc1990","pc1997","pc2002","pc2006","eduyr","pc2012","formal",
              "pc2018")
ladder_vars <- c("adladdereconomic","adladdercommunity")
outcome_vars <- c("srq","ravens","happiness")
order_vars = c(early_life_vars,ses_vars,ladder_vars,outcome_vars)


# Models --------------

path_model_srq1  <- '
srq ~  pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

path_model_srq2c <- '
srq ~  adladdercommunity + pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
adladdercommunity ~ pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

path_model_srq2e <- '
srq ~  adladdereconomic + pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
adladdereconomic ~ pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

path_model_happiness1  <- '
happiness ~  pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

path_model_happiness2c <- '
happiness ~  adladdercommunity + pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
adladdercommunity ~ pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

path_model_happiness2e <- '
happiness ~  adladdereconomic + pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
adladdereconomic ~ pc2018 + formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2018 ~ formal + pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
formal ~ pc2012 + eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2012 ~ eduyr + pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
eduyr ~ pc2006 + pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2006 ~ pc2002 + pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc2002 ~ pc1997 + pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1997 ~ pc1990 + moscho_imputed + moage_imputed + black + chbirtho + male
pc1990 ~ moscho_imputed + moage_imputed
'

