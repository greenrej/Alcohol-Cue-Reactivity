
# load relevant packages
library(tidyverse) 
library(psych) 
library(DescTools) 
library(ggplot2) 
library(purrr)
library(lubridate) 
library(lme4) 
library(lmerTest) 
library(performance) 
library(car) 
library(lmtest) 
library(lattice) 
library(DHARMa)
library(mice)
library(broom.mixed)
library(Hmisc)
library(broom)
library(RColorBrewer)
library(effectsize)
library(effsize)
library(CR2)
library(rstatix)

# Finalize Demographic Covariates: Full sample --------------------------------
# LRT for demographic covariates 
# labels: _1 = age, _2 = sex, _3= race 

#------------------------------------------------------------------------------#
#                               Full Models
#------------------------------------------------------------------------------#

# ROI: ACC_Mid
ACC_Mid_1 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

ACC_Mid_2 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

ACC_Mid_3 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Amygdala - L
Amygdala_L_1 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_2 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_3 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Amygdala - R
Amygdala_R_1 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_2 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_3 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Caudate - L 
Caudate_L_1 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_2 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_3 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Caudate - R 
Caudate_R_1 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_2 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_3 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Insula - L 
Insula_L_1 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Insula_L_2 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Insula_L_3 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Insula - R 
Insula_R_1 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Insula_R_2 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Insula_R_3 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Nacc - L 
Nacc_L_1 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_2 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_3 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Nacc - R
Nacc_R_1 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_2 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_3 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Putamen - L 
Putamen_L_1 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_2 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_3 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# ROI: Putamen - R 
Putamen_R_1 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_2 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_3 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# subjective craving: AOCDS
AOCDS_1 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

AOCDS_2 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

AOCDS_3 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# subjective craving: ACQ
ACQ_1 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

ACQ_2 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

ACQ_3 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# TLFB alcohol use: dpdd
dpdd_1 <- lmer(
  dpdd ~ Med + Visit + Seq + dpdd_base + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

dpdd_2 <- lmer(
  dpdd ~ Med + Visit + Seq + dpdd_base + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

dpdd_3 <- lmer(
  dpdd ~ Med + Visit + Seq + dpdd_base + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# TLFB alcohol use: total drinks
drinks_sum_1 <- lmer(
  drinks_sum ~ Med + Visit + Seq + drinks_sum_base + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

drinks_sum_2 <- lmer(
  drinks_sum ~ Med + Visit + Seq + drinks_sum_base + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

drinks_sum_3 <- lmer(
  drinks_sum ~ Med + Visit + Seq + drinks_sum_base + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# TLFB alcohol use: drinking days
drinking_days_1 <- lmer(
  drinking_days ~ Med + Visit + Seq + drinking_days_base + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

drinking_days_2 <- lmer(
  drinking_days ~ Med + Visit + Seq + drinking_days_base + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

drinking_days_3 <- lmer(
  drinking_days ~ Med + Visit + Seq + drinking_days_base + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# TLFB alcohol use: binge
binge_1 <- lmer(
  binge ~ Med + Visit + Seq + binge_base + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l)

binge_2 <- lmer(
  binge ~ Med + Visit + Seq + binge_base + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

binge_3 <- lmer(
  binge ~ Med + Visit + Seq + binge_base + cannabis_days_base + 
    nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                             Reduced Models
#------------------------------------------------------------------------------#    

ACC_Mid_red <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Amygdala_L_red <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Amygdala_R_red <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Caudate_L_red <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base +
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Caudate_R_red <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Insula_L_red <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Insula_R_red <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Nacc_L_red <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Nacc_R_red <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Putamen_L_red <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

Putamen_R_red <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

AOCDS_red <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

ACQ_red <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

dpdd_red <- lmer(
  dpdd ~ Med + Visit + Seq + dpdd_base + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

drinks_sum_red <- lmer(
  drinks_sum ~ Med + Visit + Seq + drinks_sum_base + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

drinking_days_red <- lmer(
  drinking_days ~ Med + Visit + Seq + drinking_days_base + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

binge_red <- lmer(
  binge ~ Med + Visit + Seq + binge_base + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                             LRT
#------------------------------------------------------------------------------#    

lrtest(ACC_Mid_1, ACC_Mid_red)
lrtest(ACC_Mid_2, ACC_Mid_red) 
lrtest(ACC_Mid_3, ACC_Mid_red) 

lrtest(Amygdala_L_1, Amygdala_L_red) 
lrtest(Amygdala_L_2, Amygdala_L_red) 
lrtest(Amygdala_L_3, Amygdala_L_red) 

lrtest(Amygdala_R_1, Amygdala_R_red) 
lrtest(Amygdala_R_2, Amygdala_R_red)  
lrtest(Amygdala_R_3, Amygdala_R_red) 

lrtest(Caudate_L_1, Caudate_L_red) 
lrtest(Caudate_L_2, Caudate_L_red) 
lrtest(Caudate_L_3, Caudate_L_red) 

lrtest(Caudate_R_1, Caudate_R_red) 
lrtest(Caudate_R_2, Caudate_R_red) 
lrtest(Caudate_R_3, Caudate_R_red)

lrtest(Insula_L_1, Insula_L_red) 
lrtest(Insula_L_2, Insula_L_red)
lrtest(Insula_L_3, Insula_L_red)

lrtest(Insula_R_1, Insula_R_red) 
lrtest(Insula_R_2, Insula_R_red) 
lrtest(Insula_R_3, Insula_R_red) 

lrtest(Nacc_L_1, Nacc_L_red) 
lrtest(Nacc_L_2, Nacc_L_red) 
lrtest(Nacc_L_3, Nacc_L_red) 

lrtest(Nacc_R_1, Nacc_R_red) 
lrtest(Nacc_R_2, Nacc_R_red) 
lrtest(Nacc_R_3, Nacc_R_red) 

lrtest(Putamen_L_1, Putamen_L_red) 
lrtest(Putamen_L_2, Putamen_L_red)
lrtest(Putamen_L_3, Putamen_L_red) 

lrtest(Putamen_R_1, Putamen_R_red) 
lrtest(Putamen_R_2, Putamen_R_red)
lrtest(Putamen_R_3, Putamen_R_red) 

lrtest(AOCDS_1, AOCDS_red) 
lrtest(AOCDS_2, AOCDS_red) 
lrtest(AOCDS_3, AOCDS_red) 

lrtest(ACQ_1, ACQ_red) 
lrtest(ACQ_2, ACQ_red) 
lrtest(ACQ_3, ACQ_red) 

lrtest(dpdd_1, dpdd_red) 
lrtest(dpdd_2, dpdd_red) 
lrtest(dpdd_3, dpdd_red) 

lrtest(drinks_sum_1, drinks_sum_red) 
lrtest(drinks_sum_2, drinks_sum_red)
lrtest(drinks_sum_3, drinks_sum_red) 

lrtest(drinking_days_1, drinking_days_red) 
lrtest(drinking_days_2, drinking_days_red) 
lrtest(drinking_days_3, drinking_days_red) 

lrtest(binge_1, binge_red) 
lrtest(binge_2, binge_red) 
lrtest(binge_3, binge_red) 

# drop objects no longer needed 
rm(ACC_Mid_1, ACC_Mid_2, ACC_Mid_3, 
   Amygdala_L_1, Amygdala_L_2, Amygdala_L_3, 
   Amygdala_R_1, Amygdala_R_2, Amygdala_R_3,
   Caudate_L_1, Caudate_L_2, Caudate_L_3,
   Caudate_R_1, Caudate_R_2, Caudate_R_3, 
   Insula_L_1, Insula_L_2, Insula_L_3,
   Insula_R_1, Insula_R_2, Insula_R_3, 
   Nacc_L_1, Nacc_L_2, Nacc_L_3, 
   Nacc_R_1, Nacc_R_2, Nacc_R_3, 
   Putamen_L_1, Putamen_L_2, Putamen_L_3, 
   Putamen_R_1, Putamen_R_2, Putamen_R_3,
   AOCDS_1, AOCDS_2, AOCDS_3, 
   ACQ_1, ACQ_2, ACQ_3,
   dpdd_1, dpdd_2, dpdd_3,
   drinks_sum_1, drinks_sum_2, drinks_sum_3,
   drinking_days_1, drinking_days_2, drinking_days_3,
   binge_1, binge_2, binge_3,
   ACC_Mid_red, Amygdala_L_red, Amygdala_R_red, Caudate_L_red,
   Caudate_R_red, Insula_L_red, Insula_R_red, Nacc_L_red, 
   Nacc_R_red, Putamen_L_red, Putamen_R_red,
   AOCDS_red, ACQ_red,
   dpdd_red, drinks_sum_red, drinking_days_red, binge_red)



# Finalize Demographic Covariates: AUD subgroup------------------------------

#------------------------------------------------------------------------------#
#                               Full Models
#------------------------------------------------------------------------------#

# ROI: ACC_Mid
ACC_Mid_1 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

ACC_Mid_2 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Amygdala - L
Amygdala_L_1 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_2 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Amygdala - R
Amygdala_R_1 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_2 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Caudate - L 
Caudate_L_1 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_L_2 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Caudate - R 
Caudate_R_1 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_R_2 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Insula - L 
Insula_L_1 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_2 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Insula - R 
Insula_R_1 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_2 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Nacc - L 
Nacc_L_1 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_2 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Nacc - R
Nacc_R_1 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_2 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Putamen - L 
Putamen_L_1 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_2 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# ROI: Putamen - R 
Putamen_R_1 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_2 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# subjective craving: AOCDS
AOCDS_1 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

AOCDS_2 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# subjective craving: ACQ
ACQ_1 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + age + 
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_2 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

#------------------------------------------------------------------------------#
#                             Reduced Models
#------------------------------------------------------------------------------#    

ACC_Mid_red <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_red <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_red <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_L_red <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base +
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_R_red <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_red <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_red <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_red <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_red <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_red <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_red <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

AOCDS_red <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_red <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

#------------------------------------------------------------------------------#
#                             LRT
#------------------------------------------------------------------------------#    

lrtest(ACC_Mid_1, ACC_Mid_red)
lrtest(ACC_Mid_2, ACC_Mid_red) 

lrtest(Amygdala_L_1, Amygdala_L_red) 
lrtest(Amygdala_L_2, Amygdala_L_red) 

lrtest(Amygdala_R_1, Amygdala_R_red) 
lrtest(Amygdala_R_2, Amygdala_R_red) 

lrtest(Caudate_L_1, Caudate_L_red) 
lrtest(Caudate_L_2, Caudate_L_red) 

lrtest(Caudate_R_1, Caudate_R_red) 
lrtest(Caudate_R_2, Caudate_R_red)

lrtest(Insula_L_1, Insula_L_red) 
lrtest(Insula_L_2, Insula_L_red)

lrtest(Insula_R_1, Insula_R_red) 
lrtest(Insula_R_2, Insula_R_red) 

lrtest(Nacc_L_1, Nacc_L_red) 
lrtest(Nacc_L_2, Nacc_L_red) 

lrtest(Nacc_R_1, Nacc_R_red) 
lrtest(Nacc_R_2, Nacc_R_red) 

lrtest(Putamen_L_1, Putamen_L_red) 
lrtest(Putamen_L_2, Putamen_L_red) 

lrtest(Putamen_R_1, Putamen_R_red) 
lrtest(Putamen_R_2, Putamen_R_red) 

lrtest(AOCDS_1, AOCDS_red) 
lrtest(AOCDS_2, AOCDS_red) 

lrtest(ACQ_1, ACQ_red) 
lrtest(ACQ_2, ACQ_red) 

# drop objects no longer needed 
rm(ACC_Mid_1, ACC_Mid_2, 
   Amygdala_L_1, Amygdala_L_2, 
   Amygdala_R_1, Amygdala_R_2,
   Caudate_L_1, Caudate_L_2, 
   Caudate_R_1, Caudate_R_2,  
   Insula_L_1, Insula_L_2,
   Insula_R_1, Insula_R_2, 
   Nacc_L_1, Nacc_L_2, 
   Nacc_R_1, Nacc_R_2, 
   Putamen_L_1, Putamen_L_2,  
   Putamen_R_1, Putamen_R_2, 
   AOCDS_1, AOCDS_2, 
   ACQ_1, ACQ_2, 
   ACC_Mid_red, Amygdala_L_red, Amygdala_R_red, Caudate_L_red,
   Caudate_R_red, Insula_L_red, Insula_R_red, Nacc_L_red, 
   Nacc_R_red, Putamen_L_red, Putamen_R_red,
   AOCDS_red, ACQ_red)

# Finalize Alcohol Use Covariates: Full sample ----------------------------------------------

#------------------------------------------------------------------------------#
#                                  DPDD
#------------------------------------------------------------------------------#    

ACC_Mid_dpdd <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_dpdd <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base +
    (1 | record_id),
  df_imp_2l)

Amygdala_R_dpdd <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_dpdd <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_dpdd <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Insula_L_dpdd <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Insula_R_dpdd <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_dpdd <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_dpdd <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_dpdd <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_dpdd <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

AOCDS_dpdd <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

ACQ_dpdd <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                               Total Drinks
#------------------------------------------------------------------------------#    

ACC_Mid_drinks_sum <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_drinks_sum <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_drinks_sum <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_drinks_sum <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_drinks_sum <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Insula_L_drinks_sum <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Insula_R_drinks_sum <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_drinks_sum <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_drinks_sum <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_drinks_sum <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_drinks_sum <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

AOCDS_drinks_sum <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

ACQ_drinks_sum <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                               Drinking Days
#------------------------------------------------------------------------------#    

ACC_Mid_drinking_days <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_drinking_days <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_drinking_days <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_drinking_days <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_drinking_days <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Insula_L_drinking_days <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Insula_R_drinking_days <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_drinking_days <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_drinking_days <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_drinking_days <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_drinking_days <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

AOCDS_drinking_days <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

ACQ_drinking_days <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                                 Binge 
#------------------------------------------------------------------------------#    

ACC_Mid_binge <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_binge <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_binge <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_binge <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_binge <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Insula_L_binge <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base +
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Insula_R_binge <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_binge <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_binge <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_binge <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_binge <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)   

AOCDS_binge <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)

ACQ_binge <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l)    

#------------------------------------------------------------------------------#
#                       mini10_dependence (binary)
#------------------------------------------------------------------------------#     

ACC_Mid_mini_binary <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_mini_binary <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_mini_binary <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_mini_binary <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_mini_binary <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Insula_L_mini_binary <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Insula_R_mini_binary <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_mini_binary <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_mini_binary <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_mini_binary <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_mini_binary <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

AOCDS_mini_binary <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence +
    (1 | record_id),
  df_imp_2l)

ACQ_mini_binary <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                    mini10_dependence_a_rec (continuous)
#------------------------------------------------------------------------------#         

ACC_Mid_mini_severity <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_mini_severity <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_mini_severity <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_mini_severity <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_mini_severity <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Insula_L_mini_severity <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Insula_R_mini_severity <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_mini_severity <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_mini_severity <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_mini_severity <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_mini_severity <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)    

AOCDS_mini_severity <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)

ACQ_mini_severity <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + mini10_dependence_a_rec + 
    (1 | record_id),
  df_imp_2l)    

#------------------------------------------------------------------------------#
#                        Compare Models
#------------------------------------------------------------------------------#         

compare_performance(
  ACC_Mid_dpdd, ACC_Mid_drinks_sum, ACC_Mid_drinking_days, 
  ACC_Mid_binge, ACC_Mid_mini_binary, ACC_Mid_mini_severity, 
  rank = TRUE) 

compare_performance(
  Amygdala_L_dpdd, Amygdala_L_drinks_sum, Amygdala_L_drinking_days, 
  Amygdala_L_binge, Amygdala_L_mini_binary, Amygdala_L_mini_severity, 
  rank = TRUE) 

compare_performance(
  Amygdala_R_dpdd, Amygdala_R_drinks_sum, Amygdala_R_drinking_days, 
  Amygdala_R_binge, Amygdala_R_mini_binary, Amygdala_R_mini_severity, 
  rank = TRUE) 

compare_performance(
  Caudate_L_dpdd, Caudate_L_drinks_sum, Caudate_L_drinking_days, 
  Caudate_L_binge, Caudate_L_mini_binary, Caudate_L_mini_severity, 
  rank = TRUE) 

compare_performance(
  Caudate_R_dpdd, Caudate_R_drinks_sum, Caudate_R_drinking_days, 
  Caudate_R_binge, Caudate_R_mini_binary, Caudate_R_mini_severity, 
  rank = TRUE) 

compare_performance(
  Insula_L_dpdd, Insula_L_drinks_sum, Insula_L_drinking_days, 
  Insula_L_binge, Insula_L_mini_binary, Insula_L_mini_severity, 
  rank = TRUE) 

compare_performance(
  Insula_R_dpdd, Insula_R_drinks_sum, Insula_R_drinking_days, 
  Insula_R_binge, Insula_R_mini_binary, Insula_R_mini_severity, 
  rank = TRUE) 

compare_performance(
  Nacc_L_dpdd, Nacc_L_drinks_sum, Nacc_L_drinking_days, 
  Nacc_L_binge, Nacc_L_mini_binary, Nacc_L_mini_severity, 
  rank = TRUE) 

compare_performance(
  Nacc_R_dpdd, Nacc_R_drinks_sum, Nacc_R_drinking_days, 
  Nacc_R_binge, Nacc_R_mini_binary, Nacc_R_mini_severity, 
  rank = TRUE) 

compare_performance(
  Putamen_L_dpdd, Putamen_L_drinks_sum, Putamen_L_drinking_days, 
  Putamen_L_binge, Putamen_L_mini_binary, Putamen_L_mini_severity, 
  rank = TRUE) 

compare_performance(
  Putamen_R_dpdd, Putamen_R_drinks_sum, Putamen_R_drinking_days, 
  Putamen_R_binge, Putamen_R_mini_binary, Putamen_R_mini_severity, 
  rank = TRUE) 

compare_performance(
  AOCDS_dpdd, AOCDS_drinks_sum, AOCDS_drinking_days, 
  AOCDS_binge, AOCDS_mini_binary, AOCDS_mini_severity, 
  rank = TRUE) 

compare_performance(
  ACQ_dpdd, ACQ_drinks_sum, ACQ_drinking_days, 
  ACQ_binge, ACQ_mini_binary, ACQ_mini_severity, 
  rank = TRUE)

rm(ACC_Mid_dpdd, ACC_Mid_drinks_sum, ACC_Mid_drinking_days, 
   ACC_Mid_binge, ACC_Mid_mini_binary, ACC_Mid_mini_severity,
   Amygdala_L_dpdd, Amygdala_L_drinks_sum, Amygdala_L_drinking_days, 
   Amygdala_L_binge, Amygdala_L_mini_binary, Amygdala_L_mini_severity,
   Amygdala_R_dpdd, Amygdala_R_drinks_sum, Amygdala_R_drinking_days, 
   Amygdala_R_binge, Amygdala_R_mini_binary, Amygdala_R_mini_severity,
   Caudate_L_dpdd, Caudate_L_drinks_sum, Caudate_L_drinking_days, 
   Caudate_L_binge, Caudate_L_mini_binary, Caudate_L_mini_severity,
   Caudate_R_dpdd, Caudate_R_drinks_sum, Caudate_R_drinking_days, 
   Caudate_R_binge, Caudate_R_mini_binary, Caudate_R_mini_severity,
   Insula_L_dpdd, Insula_L_drinks_sum, Insula_L_drinking_days, 
   Insula_L_binge, Insula_L_mini_binary, Insula_L_mini_severity, 
   Insula_R_dpdd, Insula_R_drinks_sum, Insula_R_drinking_days, 
   Insula_R_binge, Insula_R_mini_binary, Insula_R_mini_severity, 
   Nacc_L_dpdd, Nacc_L_drinks_sum, Nacc_L_drinking_days, 
   Nacc_L_binge, Nacc_L_mini_binary, Nacc_L_mini_severity, 
   Nacc_R_dpdd, Nacc_R_drinks_sum, Nacc_R_drinking_days, 
   Nacc_R_binge, Nacc_R_mini_binary, Nacc_R_mini_severity, 
   Putamen_L_dpdd, Putamen_L_drinks_sum, Putamen_L_drinking_days, 
   Putamen_L_binge, Putamen_L_mini_binary, Putamen_L_mini_severity,
   Putamen_R_dpdd, Putamen_R_drinks_sum, Putamen_R_drinking_days, 
   Putamen_R_binge, Putamen_R_mini_binary, Putamen_R_mini_severity,
   AOCDS_dpdd, AOCDS_drinks_sum, AOCDS_drinking_days, 
   AOCDS_binge, AOCDS_mini_binary, AOCDS_mini_severity,
   ACQ_dpdd, ACQ_drinks_sum, ACQ_drinking_days, 
   ACQ_binge, ACQ_mini_binary, ACQ_mini_severity)    



# Finalize Alcohol Use Covariates: AUD subgroup --------------------------------

#------------------------------------------------------------------------------#
#                                  DPDD
#------------------------------------------------------------------------------#    

ACC_Mid_dpdd <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_dpdd <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base +
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_dpdd <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

Caudate_L_dpdd <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

Caudate_R_dpdd <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_dpdd <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_dpdd <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_dpdd <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_dpdd <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_dpdd <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_dpdd <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

AOCDS_dpdd <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_dpdd <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + dpdd_base + 
    (1 | record_id),
  df_imp_2l_AUD)

#------------------------------------------------------------------------------#
#                               Total Drinks
#------------------------------------------------------------------------------#    

ACC_Mid_drinks_sum <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_drinks_sum <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_drinks_sum <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_L_drinks_sum <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_R_drinks_sum <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_drinks_sum <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_drinks_sum <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_drinks_sum <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_drinks_sum <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_drinks_sum <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_drinks_sum <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

AOCDS_drinks_sum <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_drinks_sum <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + drinks_sum_base + 
    (1 | record_id),
  df_imp_2l_AUD)

#------------------------------------------------------------------------------#
#                               Drinking Days
#------------------------------------------------------------------------------#    

ACC_Mid_drinking_days <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_drinking_days <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_drinking_days <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_L_drinking_days <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_R_drinking_days <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_drinking_days <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_drinking_days <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_drinking_days <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_drinking_days <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_drinking_days <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_drinking_days <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

AOCDS_drinking_days <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_drinking_days <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + drinking_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

#------------------------------------------------------------------------------#
#                                 Binge 
#------------------------------------------------------------------------------#    

ACC_Mid_binge <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_L_binge <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Amygdala_R_binge <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_L_binge <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Caudate_R_binge <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_L_binge <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days_base +
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Insula_R_binge <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_L_binge <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Nacc_R_binge <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_L_binge <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

Putamen_R_binge <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)   

AOCDS_binge <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)

ACQ_binge <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days_base + 
    nicotine_days_base + binge_base + 
    (1 | record_id),
  df_imp_2l_AUD)    

#------------------------------------------------------------------------------#
#                        Compare Models
#------------------------------------------------------------------------------#         

compare_performance(
  ACC_Mid_dpdd, ACC_Mid_drinks_sum, ACC_Mid_drinking_days, 
  ACC_Mid_binge, rank = TRUE) 

compare_performance(
  Amygdala_L_dpdd, Amygdala_L_drinks_sum, Amygdala_L_drinking_days, 
  Amygdala_L_binge, rank = TRUE) 

compare_performance(
  Amygdala_R_dpdd, Amygdala_R_drinks_sum, Amygdala_R_drinking_days, 
  Amygdala_R_binge, rank = TRUE) 

compare_performance(
  Caudate_L_dpdd, Caudate_L_drinks_sum, Caudate_L_drinking_days, 
  Caudate_L_binge, rank = TRUE) 

compare_performance(
  Caudate_R_dpdd, Caudate_R_drinks_sum, Caudate_R_drinking_days, 
  Caudate_R_binge, rank = TRUE)

compare_performance(
  Insula_L_dpdd, Insula_L_drinks_sum, Insula_L_drinking_days, 
  Insula_L_binge, rank = TRUE)

compare_performance(
  Insula_R_dpdd, Insula_R_drinks_sum, Insula_R_drinking_days, 
  Insula_R_binge, rank = TRUE) 

compare_performance(
  Nacc_L_dpdd, Nacc_L_drinks_sum, Nacc_L_drinking_days, 
  Nacc_L_binge, rank = TRUE)

compare_performance(
  Nacc_R_dpdd, Nacc_R_drinks_sum, Nacc_R_drinking_days, 
  Nacc_R_binge, rank = TRUE)

compare_performance(
  Putamen_L_dpdd, Putamen_L_drinks_sum, Putamen_L_drinking_days, 
  Putamen_L_binge, rank = TRUE) 

compare_performance(
  Putamen_R_dpdd, Putamen_R_drinks_sum, Putamen_R_drinking_days, 
  Putamen_R_binge, rank = TRUE) 

compare_performance(
  AOCDS_dpdd, AOCDS_drinks_sum, AOCDS_drinking_days, 
  AOCDS_binge, rank = TRUE) 

compare_performance(
  ACQ_dpdd, ACQ_drinks_sum, ACQ_drinking_days, 
  ACQ_binge, rank = TRUE)

rm(ACC_Mid_dpdd, ACC_Mid_drinks_sum, ACC_Mid_drinking_days, 
   ACC_Mid_binge, 
   Amygdala_L_dpdd, Amygdala_L_drinks_sum, Amygdala_L_drinking_days, 
   Amygdala_L_binge, 
   Amygdala_R_dpdd, Amygdala_R_drinks_sum, Amygdala_R_drinking_days, 
   Amygdala_R_binge, 
   Caudate_L_dpdd, Caudate_L_drinks_sum, Caudate_L_drinking_days, 
   Caudate_L_binge,
   Caudate_R_dpdd, Caudate_R_drinks_sum, Caudate_R_drinking_days, 
   Caudate_R_binge, 
   Insula_L_dpdd, Insula_L_drinks_sum, Insula_L_drinking_days, 
   Insula_L_binge, 
   Insula_R_dpdd, Insula_R_drinks_sum, Insula_R_drinking_days, 
   Insula_R_binge, 
   Nacc_L_dpdd, Nacc_L_drinks_sum, Nacc_L_drinking_days, 
   Nacc_L_binge,
   Nacc_R_dpdd, Nacc_R_drinks_sum, Nacc_R_drinking_days, 
   Nacc_R_binge,
   Putamen_L_dpdd, Putamen_L_drinks_sum, Putamen_L_drinking_days, 
   Putamen_L_binge, 
   Putamen_R_dpdd, Putamen_R_drinks_sum, Putamen_R_drinking_days, 
   Putamen_R_binge, 
   AOCDS_dpdd, AOCDS_drinks_sum, AOCDS_drinking_days, 
   AOCDS_binge,
   ACQ_dpdd, ACQ_drinks_sum, ACQ_drinking_days, 
   ACQ_binge) 

