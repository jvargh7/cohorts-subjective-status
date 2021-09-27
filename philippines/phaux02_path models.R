
# Variables ---------

early_life_vars <- c("moage","chbirtho","male")
ses_vars <- c("moscho","pc1983","pc1991","pc1994","pc2002","eduyr","pc2005","pc2009","rural2018","formal",
              "pc2018")
ladder_vars <- c("adladdereconomic","adladdercommunity")
outcome_vars <- c("bmi","srq","ravens","happiness")
order_vars = c(early_life_vars,ses_vars,ladder_vars,outcome_vars)


# Models --------------

path_model_bmi1  <- '
bmi ~  pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_bmi2c <- '
bmi ~  adladdercommunity + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdercommunity ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_bmi2e <- '
bmi ~  adladdereconomic + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdereconomic ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_srq1  <- '
srq ~  pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_srq2c <- '
srq ~  adladdercommunity + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdercommunity ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_srq2e <- '
srq ~  adladdereconomic + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdereconomic ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_happiness1  <- '
happiness ~  pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_happiness2c <- '
happiness ~  adladdercommunity + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdercommunity ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

path_model_happiness2e <- '
happiness ~  adladdereconomic + pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
adladdereconomic ~ pc2018 + formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2018 ~ formal + rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
formal ~ rural2018 + pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
rural2018 ~ pc2009 + pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2009 ~ pc2005 + pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
pc2005 ~ pc2002 + pc1994 + pc1991 +pc1983 + moscho + moage + chbirtho + male + eduyr
eduyr ~ pc2002 + pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc2002 ~ pc1994 + pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1994 ~ pc1991 + pc1983 + moscho + moage + chbirtho + male
pc1991 ~ pc1983 + moscho + moage + chbirtho + male
pc1983 ~ moscho + moage
'

