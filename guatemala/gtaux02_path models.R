
# Variables ---------

early_life_vars <- c("gtatole","full","atolefull","moage_imputed","chbirtho_imputed","male","byear")
ses_vars <- c("moscho_imputed","pcall6775_1","eduyr","pcall1987_1","pcall1996_1","pcall2002_1","rural2016","rural2018","rural",
              "formal","pcall1618_1")
ladder_vars <- c("adladdereconomic","adladdercommunity")
outcome_vars <- c("bmi","srq","ravens","happiness")
order_vars = c(early_life_vars,ses_vars,ladder_vars,outcome_vars)


# Models --------------

path_model_bmi1  <- '
bmi ~  pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_bmi2c <- '
bmi ~  adladdercommunity + pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdercommunity ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_bmi2e <- '
bmi ~  adladdereconomic + pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdereconomic ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_srq1  <- '
srq ~  pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_srq2c <- '
srq ~  adladdercommunity + pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdercommunity ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_srq2e <- '
srq ~  adladdereconomic + pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdereconomic ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_happiness1  <- '
happiness ~  pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_happiness2c <- '
happiness ~  adladdercommunity + pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdercommunity ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

path_model_happiness2e <- '
happiness ~  adladdereconomic + pcall1618_1 + formal + rural2018 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
adladdereconomic ~ pcall1618_1 + formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1618_1 ~ formal + rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
formal ~ rural2016 + pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
rural2016 ~ pcall2002_1 + pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall2002_1 ~ pcall1996_1 + pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1996_1 ~ pcall1987_1 + pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
pcall1987_1 ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear + eduyr
eduyr ~ pcall6775_1 + moscho_imputed + gtatole + full + atolefull + moage_imputed + chbirtho_imputed + male + byear
pcall6775_1 ~ moscho_imputed + gtatole + full + atolefull + moage_imputed + byear
'

