# Load Relevant Libraries -------------------------------------------------
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
  
# Finalize Demographic Covariates ----------------------------------------------

# LRT for demographic covariates 
# labels: _1 = age, _2 = sex, _3= race  

#------------------------------------------------------------------------------#
#                               Full Models
#------------------------------------------------------------------------------#

# ACC_Mid
ACC_Mid_imp_full_1 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

ACC_Mid_imp_full_2 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

ACC_Mid_imp_full_3 <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Amygdala - L
Amygdala_L_imp_full_1 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_full_2 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_full_3 <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Amygdala - R
Amygdala_R_imp_full_1 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_full_2 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_full_3 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Caudate - L 
Caudate_L_imp_full_1 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_imp_full_2 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_imp_full_3 <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Caudate - R 
Caudate_R_imp_full_1 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_imp_full_2 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_imp_full_3 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Insula - L 
Insula_L_imp_full_1 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Insula_L_imp_full_2 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Insula_L_imp_full_3 <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Insula - R 
Insula_R_imp_full_1 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Insula_R_imp_full_2 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Insula_R_imp_full_3 <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Nacc - L 
Nacc_L_imp_full_1 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_imp_full_2 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_imp_full_3 <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Nacc - R
Nacc_R_imp_full_1 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_imp_full_2 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_imp_full_3 <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Putamen - L 
Putamen_L_imp_full_1 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_imp_full_2 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_imp_full_3 <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# Putamen - R 
Putamen_R_imp_full_1 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_imp_full_2 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_imp_full_3 <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# AOCDS
AOCDS_imp_full_1 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

AOCDS_imp_full_2 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

AOCDS_imp_full_3 <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

# ACQ
ACQ_imp_full_1 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + age + 
    (1 | record_id),
  df_imp_2l)

ACQ_imp_full_2 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + sex + 
    (1 | record_id),
  df_imp_2l)

ACQ_imp_full_3 <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + race + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                             Reduced Models
#------------------------------------------------------------------------------#    

ACC_Mid_imp_red <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_red <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_red <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_imp_red <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_imp_red <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Insula_L_imp_red <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Insula_R_imp_red <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_imp_red <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_imp_red <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_imp_red <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_imp_red <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

AOCDS_imp_red <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

ACQ_imp_red <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                             LRT
#------------------------------------------------------------------------------#    

lrtest(ACC_Mid_imp_full_1, ACC_Mid_imp_red)
lrtest(ACC_Mid_imp_full_2, ACC_Mid_imp_red) 
lrtest(ACC_Mid_imp_full_3, ACC_Mid_imp_red) 

lrtest(Amygdala_L_imp_full_1, Amygdala_L_imp_red) 
lrtest(Amygdala_L_imp_full_2, Amygdala_L_imp_red) 
lrtest(Amygdala_L_imp_full_3, Amygdala_L_imp_red) 

lrtest(Amygdala_R_imp_full_1, Amygdala_R_imp_red) 
lrtest(Amygdala_R_imp_full_2, Amygdala_R_imp_red)  
lrtest(Amygdala_R_imp_full_3, Amygdala_R_imp_red) 

lrtest(Caudate_L_imp_full_1, Caudate_L_imp_red) 
lrtest(Caudate_L_imp_full_2, Caudate_L_imp_red) # significant
lrtest(Caudate_L_imp_full_3, Caudate_L_imp_red) 

lrtest(Caudate_R_imp_full_1, Caudate_R_imp_red) 
lrtest(Caudate_R_imp_full_2, Caudate_R_imp_red) 
lrtest(Caudate_R_imp_full_3, Caudate_R_imp_red)

lrtest(Insula_L_imp_full_1, Insula_L_imp_red) 
lrtest(Insula_L_imp_full_2, Insula_L_imp_red)
lrtest(Insula_L_imp_full_3, Insula_L_imp_red)

lrtest(Insula_R_imp_full_1, Insula_R_imp_red) 
lrtest(Insula_R_imp_full_2, Insula_R_imp_red) 
lrtest(Insula_R_imp_full_3, Insula_R_imp_red) 

lrtest(Nacc_L_imp_full_1, Nacc_L_imp_red) 
lrtest(Nacc_L_imp_full_2, Nacc_L_imp_red) 
lrtest(Nacc_L_imp_full_3, Nacc_L_imp_red) 

lrtest(Nacc_R_imp_full_1, Nacc_R_imp_red) 
lrtest(Nacc_R_imp_full_2, Nacc_R_imp_red) 
lrtest(Nacc_R_imp_full_3, Nacc_R_imp_red) 

lrtest(Putamen_L_imp_full_1, Putamen_L_imp_red) 
lrtest(Putamen_L_imp_full_2, Putamen_L_imp_red) # significant
lrtest(Putamen_L_imp_full_3, Putamen_L_imp_red) 

lrtest(Putamen_R_imp_full_1, Putamen_R_imp_red) 
lrtest(Putamen_R_imp_full_2, Putamen_R_imp_red) # significant
lrtest(Putamen_R_imp_full_3, Putamen_R_imp_red) 

lrtest(AOCDS_imp_full_1, AOCDS_imp_red) 
lrtest(AOCDS_imp_full_2, AOCDS_imp_red) 
lrtest(AOCDS_imp_full_3, AOCDS_imp_red) # significant

lrtest(ACQ_imp_full_1, ACQ_imp_red) # significant
lrtest(ACQ_imp_full_2, ACQ_imp_red) # significant
lrtest(ACQ_imp_full_3, ACQ_imp_red) # significant

# drop objects no longer needed 
rm(ACC_Mid_imp_full_1, ACC_Mid_imp_full_2, ACC_Mid_imp_full_3, 
   Amygdala_L_imp_full_1, Amygdala_L_imp_full_2, Amygdala_L_imp_full_3,
   Amygdala_R_imp_full_1, Amygdala_R_imp_full_2, Amygdala_R_imp_full_3, 
   Caudate_L_imp_full_1, Caudate_L_imp_full_2, Caudate_L_imp_full_3, 
   Caudate_R_imp_full_1, Caudate_R_imp_full_2, Caudate_R_imp_full_3,
   Insula_L_imp_full_1, Insula_L_imp_full_2, Insula_L_imp_full_3, 
   Insula_R_imp_full_1, Insula_R_imp_full_2, Insula_R_imp_full_3, 
   Nacc_L_imp_full_1, Nacc_L_imp_full_2, Nacc_L_imp_full_3, 
   Nacc_R_imp_full_1, Nacc_R_imp_full_2, Nacc_R_imp_full_3,
   Putamen_L_imp_full_1, Putamen_L_imp_full_2, Putamen_L_imp_full_3, 
   Putamen_R_imp_full_1, Putamen_R_imp_full_2, Putamen_R_imp_full_3,
   AOCDS_imp_full_1, AOCDS_imp_full_2, AOCDS_imp_full_3,
   ACQ_imp_full_1, ACQ_imp_full_2, ACQ_imp_full_3,
   ACC_Mid_imp_red, Amygdala_L_imp_red, Amygdala_R_imp_red, Caudate_L_imp_red,
   Caudate_R_imp_red, Insula_L_imp_red, Insula_R_imp_red, Nacc_L_imp_red, 
   Nacc_R_imp_red, Putamen_L_imp_red, Putamen_R_imp_red,
   AOCDS_imp_red, ACQ_imp_red)

# Finalize Alcohol Use Covariates ------------------------------------------

#------------------------------------------------------------------------------#
#                       Drinks per Drinking Day (DPDD)
#------------------------------------------------------------------------------#    

ACC_Mid_imp_dpdd <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_dpdd <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + dpdd +
    (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_dpdd <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Caudate_L_imp_dpdd <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Caudate_R_imp_dpdd <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Insula_L_imp_dpdd <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Insula_R_imp_dpdd <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Nacc_L_imp_dpdd <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Nacc_R_imp_dpdd <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Putamen_L_imp_dpdd <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

Putamen_R_imp_dpdd <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

AOCDS_imp_dpdd <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

ACQ_imp_dpdd <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                               Total Drinks
#------------------------------------------------------------------------------#    

ACC_Mid_imp_drinks_sum <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_drinks_sum <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_drinks_sum <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Caudate_L_imp_drinks_sum <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Caudate_R_imp_drinks_sum <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Insula_L_imp_drinks_sum <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Insula_R_imp_drinks_sum <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Nacc_L_imp_drinks_sum <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Nacc_R_imp_drinks_sum <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Putamen_L_imp_drinks_sum <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

Putamen_R_imp_drinks_sum <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

AOCDS_imp_drinks_sum <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

ACQ_imp_drinks_sum <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                               Drinking Days
#------------------------------------------------------------------------------#    

ACC_Mid_imp_drinking_days <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_drinking_days <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_drinking_days <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Caudate_L_imp_drinking_days <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Caudate_R_imp_drinking_days <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Insula_L_imp_drinking_days <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Insula_R_imp_drinking_days <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Nacc_L_imp_drinking_days <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Nacc_R_imp_drinking_days <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Putamen_L_imp_drinking_days <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

Putamen_R_imp_drinking_days <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

AOCDS_imp_drinking_days <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

ACQ_imp_drinking_days <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                           Binge Drinking Days
#------------------------------------------------------------------------------#    

ACC_Mid_imp_binge <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_binge <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_binge <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Caudate_L_imp_binge <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Caudate_R_imp_binge <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Insula_L_imp_binge <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days +
    binge + (1 | record_id),
  df_imp_2l)

Insula_R_imp_binge <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days +
    binge + (1 | record_id),
  df_imp_2l)

Nacc_L_imp_binge <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Nacc_R_imp_binge <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

Putamen_L_imp_binge <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days +
    binge + (1 | record_id),
  df_imp_2l)

Putamen_R_imp_binge <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)   

AOCDS_imp_binge <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)

ACQ_imp_binge <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    binge + (1 | record_id),
  df_imp_2l)    

#------------------------------------------------------------------------------#
#                       mini10_dependence (binary)
#------------------------------------------------------------------------------#     

ACC_Mid_imp_mini_binary <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_mini_binary <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_mini_binary <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Caudate_L_imp_mini_binary <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days +
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Caudate_R_imp_mini_binary <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Insula_L_imp_mini_binary <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Insula_R_imp_mini_binary <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Nacc_L_imp_mini_binary <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Nacc_R_imp_mini_binary <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Putamen_L_imp_mini_binary <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

Putamen_R_imp_mini_binary <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

AOCDS_imp_mini_binary <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

ACQ_imp_mini_binary <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)

#------------------------------------------------------------------------------#
#                    mini10_dependence_a_rec (continuous)
#------------------------------------------------------------------------------#         

ACC_Mid_imp_mini_severity <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Amygdala_L_imp_mini_severity <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Amygdala_R_imp_mini_severity <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Caudate_L_imp_mini_severity <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Caudate_R_imp_mini_severity <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Insula_L_imp_mini_severity <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Insula_R_imp_mini_severity <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Nacc_L_imp_mini_severity <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Nacc_R_imp_mini_severity <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Putamen_L_imp_mini_severity <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

Putamen_R_imp_mini_severity <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)    

AOCDS_imp_mini_severity <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)

ACQ_imp_mini_severity <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)    

#------------------------------------------------------------------------------#
#                        Compare Models
#------------------------------------------------------------------------------#         

compare_performance(
  ACC_Mid_imp_dpdd, ACC_Mid_imp_drinks_sum, ACC_Mid_imp_drinking_days, 
  ACC_Mid_imp_binge, ACC_Mid_imp_mini_binary, ACC_Mid_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Amygdala_L_imp_dpdd, Amygdala_L_imp_drinks_sum, Amygdala_L_imp_drinking_days, 
  Amygdala_L_imp_binge, Amygdala_L_imp_mini_binary, Amygdala_L_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Amygdala_R_imp_dpdd, Amygdala_R_imp_drinks_sum, Amygdala_R_imp_drinking_days, 
  Amygdala_R_imp_binge, Amygdala_R_imp_mini_binary, Amygdala_R_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Caudate_L_imp_dpdd, Caudate_L_imp_drinks_sum, Caudate_L_imp_drinking_days, 
  Caudate_L_imp_binge, Caudate_L_imp_mini_binary, Caudate_L_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Caudate_R_imp_dpdd, Caudate_R_imp_drinks_sum, Caudate_R_imp_drinking_days, 
  Caudate_R_imp_binge, Caudate_R_imp_mini_binary, Caudate_R_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Insula_L_imp_dpdd, Insula_L_imp_drinks_sum, Insula_L_imp_drinking_days, 
  Insula_L_imp_binge, Insula_L_imp_mini_binary, Insula_L_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Insula_R_imp_dpdd, Insula_R_imp_drinks_sum, Insula_R_imp_drinking_days, 
  Insula_R_imp_binge, Insula_R_imp_mini_binary, Insula_R_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Nacc_L_imp_dpdd, Nacc_L_imp_drinks_sum, Nacc_L_imp_drinking_days, 
  Nacc_L_imp_binge, Nacc_L_imp_mini_binary, Nacc_L_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Nacc_R_imp_dpdd, Nacc_R_imp_drinks_sum, Nacc_R_imp_drinking_days, 
  Nacc_R_imp_binge, Nacc_R_imp_mini_binary, Nacc_R_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Putamen_L_imp_dpdd, Putamen_L_imp_drinks_sum, Putamen_L_imp_drinking_days, 
  Putamen_L_imp_binge, Putamen_L_imp_mini_binary, Putamen_L_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  Putamen_R_imp_dpdd, Putamen_R_imp_drinks_sum, Putamen_R_imp_drinking_days, 
  Putamen_R_imp_binge, Putamen_R_imp_mini_binary, Putamen_R_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  AOCDS_imp_dpdd, AOCDS_imp_drinks_sum, AOCDS_imp_drinking_days, 
  AOCDS_imp_binge, AOCDS_imp_mini_binary, AOCDS_imp_mini_severity, 
  rank = TRUE)

compare_performance(
  ACQ_imp_dpdd, ACQ_imp_drinks_sum, ACQ_imp_drinking_days, 
  ACQ_imp_binge, ACQ_imp_mini_binary, ACQ_imp_mini_severity, 
  rank = TRUE)

rm(ACC_Mid_imp_dpdd, ACC_Mid_imp_drinks_sum, ACC_Mid_imp_drinking_days, 
   ACC_Mid_imp_binge, ACC_Mid_imp_mini_binary, ACC_Mid_imp_mini_severity,
   Amygdala_L_imp_dpdd, Amygdala_L_imp_drinks_sum, Amygdala_L_imp_drinking_days, 
   Amygdala_L_imp_binge, Amygdala_L_imp_mini_binary, Amygdala_L_imp_mini_severity,
   Amygdala_R_imp_dpdd, Amygdala_R_imp_drinks_sum, Amygdala_R_imp_drinking_days, 
   Amygdala_R_imp_binge, Amygdala_R_imp_mini_binary, Amygdala_R_imp_mini_severity,
   Caudate_L_imp_dpdd, Caudate_L_imp_drinks_sum, Caudate_L_imp_drinking_days, 
   Caudate_L_imp_binge, Caudate_L_imp_mini_binary, Caudate_L_imp_mini_severity,
   Caudate_R_imp_dpdd, Caudate_R_imp_drinks_sum, Caudate_R_imp_drinking_days, 
   Caudate_R_imp_binge, Caudate_R_imp_mini_binary, Caudate_R_imp_mini_severity,
   Insula_L_imp_dpdd, Insula_L_imp_drinks_sum, Insula_L_imp_drinking_days, 
   Insula_L_imp_binge, Insula_L_imp_mini_binary, Insula_L_imp_mini_severity, 
   Insula_R_imp_dpdd, Insula_R_imp_drinks_sum, Insula_R_imp_drinking_days, 
   Insula_R_imp_binge, Insula_R_imp_mini_binary, Insula_R_imp_mini_severity, 
   Nacc_L_imp_dpdd, Nacc_L_imp_drinks_sum, Nacc_L_imp_drinking_days, 
   Nacc_L_imp_binge, Nacc_L_imp_mini_binary, Nacc_L_imp_mini_severity, 
   Nacc_R_imp_dpdd, Nacc_R_imp_drinks_sum, Nacc_R_imp_drinking_days, 
   Nacc_R_imp_binge, Nacc_R_imp_mini_binary, Nacc_R_imp_mini_severity, 
   Putamen_L_imp_dpdd, Putamen_L_imp_drinks_sum, Putamen_L_imp_drinking_days, 
   Putamen_L_imp_binge, Putamen_L_imp_mini_binary, Putamen_L_imp_mini_severity,
   Putamen_R_imp_dpdd, Putamen_R_imp_drinks_sum, Putamen_R_imp_drinking_days, 
   Putamen_R_imp_binge, Putamen_R_imp_mini_binary, Putamen_R_imp_mini_severity,
   AOCDS_imp_dpdd, AOCDS_imp_drinks_sum, AOCDS_imp_drinking_days, 
   AOCDS_imp_binge, AOCDS_imp_mini_binary, AOCDS_imp_mini_severity,
   ACQ_imp_dpdd, ACQ_imp_drinks_sum, ACQ_imp_drinking_days, 
   ACQ_imp_binge, ACQ_imp_mini_binary, ACQ_imp_mini_severity)    



