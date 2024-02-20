# Full Sample - Models --------------------------------------------

# Aim 1 (cue-reactivity) and aim 2 (craving)
# Exploratory: test med x sex interaction

#------------------------------------------------------------------------------#
#                           Aim #1: Cue-Reactivity
#------------------------------------------------------------------------------#

#------------------#
#    ACC - Mid 
#------------------#

# primary model
ACC_Mid_imp <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + drinks_sum + 
    (1 | record_id),
  df_imp_2l)
summary(ACC_Mid_imp)

# follow-up: significant Seq effect
temp <- df_imp_2l %>% 
  group_by(Seq) %>% 
  summarise(
    mean_ACC_Mid = mean(ACC_Mid),
    sd_ACC_Mid = sd(ACC_Mid))
temp
rm(temp)
# follow-up: significant Seq effect

# exploratory model: Med*sex interaction
ACC_Mid_imp_sex <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l) 
summary(ACC_Mid_imp_sex)

#------------------#
#   Amygdala (L)
#------------------#

# primary model
Amygdala_L_imp <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)
summary(Amygdala_L_imp)

# exploratory model: Med*sex interaction
Amygdala_L_imp_sex <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)
summary(Amygdala_L_imp_sex)

#------------------#
#   Amygdala (R)
#------------------#

# primary model
Amygdala_R_imp <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l)
summary(Amygdala_R_imp)

# exploratory model: Med*sex interaction
Amygdala_R_imp_sex <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l) 
summary(Amygdala_R_imp_sex)

#------------------#
#   Caudate (L)
#------------------#

# primary model
Caudate_L_imp <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + sex + 
    dpdd + (1 | record_id),
  df_imp_2l)
summary(Caudate_L_imp)

# exploratory model: Med*sex interaction
Caudate_L_imp_sex <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l)
summary(Caudate_L_imp_sex)

#------------------#
#   Caudate (R)
#------------------#

# primary model
Caudate_R_imp <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l) 
summary(Caudate_R_imp)

# exploratory model: Med*sex interaction
Caudate_R_imp_sex <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l) 
summary(Caudate_R_imp_sex)

#------------------#
#   Insula (L)
#------------------#

# primary model
Insula_L_imp <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l) 
summary(Insula_L_imp)

# exploratory model: Med*sex interaction
Insula_L_imp_sex <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l) 
summary(Insula_L_imp_sex)

#------------------#
#   Insula (R)
#------------------#

# primary model
Insula_R_imp <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)
summary(Insula_R_imp)

# exploratory model: Med*sex interaction
Insula_R_imp_sex <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)
summary(Insula_R_imp_sex)

#----------------------#
# Nucleus Accumbens (L)
#----------------------#

# primary model
Nacc_L_imp <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l) 
summary(Nacc_L_imp)

# exploratory model: Med*sex interaction
Nacc_L_imp_sex <- lmer(
  Nacc_L ~ Med*sex + Visit + Seq + Nacc_L_Baseline + cannabis_days + dpdd + 
    (1 | record_id),
  df_imp_2l) 
summary(Nacc_L_imp_sex)

#----------------------#
# Nucleus Accumbens (R)
#----------------------#

# primary model
Nacc_R_imp <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l) 
summary(Nacc_R_imp)

# exploratory model: Med*sex interaction
Nacc_R_imp_sex <- lmer(
  Nacc_R ~ Med*sex + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    mini10_dependence + (1 | record_id),
  df_imp_2l) 
summary(Nacc_R_imp_sex)

#------------------#
#   Putamen (L)
#------------------#

# primary model
Putamen_L_imp <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + sex + 
    dpdd + (1 | record_id),
  df_imp_2l)
summary(Putamen_L_imp)

# exploratory model: Med*sex interaction
Putamen_L_imp_sex <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l) 
summary(Putamen_L_imp_sex)

#------------------#
#   Putamen (R)
#------------------#

# primary model
Putamen_R_imp <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + sex + 
    dpdd + (1 | record_id),
  df_imp_2l)
summary(Putamen_R_imp)

# exploratory model: Med*sex interaction
Putamen_R_imp_sex <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l) 
summary(Putamen_R_imp_sex)

#------------------------------------------------------------------------------#
#                           Aim #2: Craving (self-report)
#------------------------------------------------------------------------------#

#------------------#
#      AOCDS
#------------------#

# primary model
AOCDS_imp <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + race + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)
summary(AOCDS_imp)

# follow-up: significant Med effect
AOCDS_Med <- df_imp_2l %>% 
  group_by(Med) %>% 
  summarise(
    mean_AOCDS = mean(AOCDS),
    sd_AOCDS = sd(AOCDS))
AOCDS_Med
rm(AOCDS_Med)

# exploratory model: Med*sex interaction
AOCDS_imp_sex <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + cannabis_days + race + 
    mini10_dependence + (1 | record_id),
  df_imp_2l)
summary(AOCDS_imp_sex)

#------------------#
#      ACQ
#------------------#

# primary model
ACQ_imp <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + age + sex + race + 
    mini10_dependence_a_rec + (1 | record_id) ,
  df_imp_2l)
summary(ACQ_imp)

# exploratory model: Med*sex interaction
ACQ_imp_sex <- lmer(
  ACQ ~ Med*sex + Visit + Seq + ACQ_Baseline + cannabis_days + age + race + 
    mini10_dependence_a_rec + (1 | record_id),
  df_imp_2l)
summary(ACQ_imp_sex)

#------------------------------------------------------------------------------#
#                       consolidate output
#------------------------------------------------------------------------------#

# coefficients from primary model
coef_output <- tidy(ACC_Mid_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACC (Mid)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation') 

amy_L <- tidy(Amygdala_L_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_R <- tidy(Amygdala_R_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_L <- tidy(Caudate_L_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_R <- tidy(Caudate_R_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_L <- tidy(Insula_L_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_R <- tidy(Insula_R_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_L <- tidy(Nacc_L_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_R <- tidy(Nacc_R_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_L <- tidy(Putamen_L_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_R <- tidy(Putamen_R_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

aocds <- tidy(AOCDS_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'AOCDS', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

acq <- tidy(ACQ_imp) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACQ', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

coef_output <- rbind(
  coef_output, amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
  put_L, put_R, aocds, acq) %>% 
  rename(
    Estimate = 'estimate',
    SE = 'std.error',
    t = 'statistic',
    p = 'p.value')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, put_L, put_R, 
   aocds, acq)

# coefficients from exploratory model
coef_sex_output <- tidy(ACC_Mid_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACC (Mid)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_L <- tidy(Amygdala_L_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_R <- tidy(Amygdala_R_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_L <- tidy(Caudate_L_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_R <- tidy(Caudate_R_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_L <- tidy(Insula_L_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_R <- tidy(Insula_R_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_L <- tidy(Nacc_L_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_R <- tidy(Nacc_R_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_L <- tidy(Putamen_L_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_R <- tidy(Putamen_R_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

aocds <- tidy(AOCDS_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'AOCDS', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

acq <- tidy(ACQ_imp_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACQ', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

coef_sex_output <- rbind(
  coef_sex_output, amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
  put_L, put_R, aocds, acq) %>% 
  rename(
    Estimate = 'estimate',
    SE = 'std.error',
    t = 'statistic',
    p = 'p.value')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, put_L, put_R, 
   aocds, acq)

# effect size from primary model for Med effect
eff_output <- eta_squared(
  ACC_Mid_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% # retain med effect size for Med effect only
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACC', variable))

amy_L <- eta_squared(
  Amygdala_L_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (L)', variable))

amy_R <- eta_squared(
  Amygdala_R_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (R)', variable))

cau_L <- eta_squared(
  Caudate_L_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (L)', variable))

cau_R <- eta_squared(
  Caudate_R_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (R)', variable))

ins_L <- eta_squared(
  Insula_L_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (L)', variable))

ins_R <- eta_squared(
  Insula_R_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (R)', variable))

nac_L <- eta_squared(
  Nacc_L_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (L)', variable))

nac_R <- eta_squared(
  Nacc_R_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (R)', variable))

put_L <- eta_squared(
  Putamen_L_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (L)', variable))

put_R <- eta_squared(
  Putamen_R_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (R)', variable))

aocds <- eta_squared(
  AOCDS_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'AOCDS', variable))

acq <- eta_squared(
  ACQ_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACQ', variable))

eff_output <- rbind(eff_output, amy_L, amy_R, cau_L, cau_R, 
                    ins_L, ins_R, nac_L, nac_R, put_L, put_R, aocds, acq) %>% 
  mutate_if(is.numeric, round, 2) 

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
   put_L, put_R, aocds, acq)

# export results
write.csv(coef_output,'output/coef_output.csv')
write.csv(coef_sex_output,'output/coef_sex_output.csv')
write.csv(eff_output,'output/eff_output.csv')

rm(coef_output, coef_sex_output, eff_output)


# Full Sample - Figures ------------------------------------------------

# df split by med and sex 
df_fig_bar <- df_imp_2l %>% 
  group_by(sex, Med) %>% 
  summarise(
    mean_Amygdala_L = mean(Amygdala_L),
    sd_Amygdala_L = sd(Amygdala_L),
    mean_Amygdala_R = mean(Amygdala_R),
    sd_Amygdala_R = sd(Amygdala_R))

# rename med
df_fig_bar <- df_fig_bar %>% 
  mutate(
    Med = case_when(
      Med == 'NAC' ~ 'N-acetylcysteine',
      Med == 'Placebo' ~ 'Placebo'))

# parameters for figure
limits_L <- aes(ymin=0.01, ymax=mean_Amygdala_L+sd_Amygdala_L)
limits_R <- aes(ymin=0.01, ymax=mean_Amygdala_R+sd_Amygdala_R)
dodge <- position_dodge(width = 0.9)

# ROI: amygdala (L)
ggplot(df_fig_bar, aes(fill=sex, y=mean_Amygdala_L, x=Med)) +
  geom_errorbar(limits_L, position = dodge, width = 0.25) +
  geom_bar(position = dodge, stat = "identity") +
  scale_fill_manual(values = c("#05C3DE", "#054C70")) +
  theme_classic(base_size = 18) +
  labs(fill = "Sex") +
  labs(x ="Medication", y = "BOLD Signal (alcohol - non-alcohol)", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial"))

# ROI: amygdala (R)
ggplot(df_fig_bar, aes(fill=sex, y=mean_Amygdala_R, x=Med)) +
  geom_errorbar(limits_R, position = dodge, width = 0.25) +
  geom_bar(position = dodge, stat = "identity") +
  scale_fill_manual(values = c("#05C3DE", "#054C70")) +
  theme_classic(base_size = 18) +
  labs(fill = "Sex") +
  labs(x ="Medication", y = "BOLD Signal (alcohol - non-alcohol)", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial"))

rm(df_fig_bar, limits_L, limits_R, dodge)

# Subgroup - Models ---------------------------------------------------------
# Identical to primary models among participants with an AUD diagnosis

# create dataset for AUD subgroup
df_imp_2l_AUD <- df_imp_2l %>% 
  subset(mini10_dependence == 'Yes')

#------------------------------------------------------------------------------#
#                           Aim #1: Cue-Reactivity
#------------------------------------------------------------------------------#

#------------------#
#    ACC - Mid 
#------------------#

# primary model
ACC_Mid_imp_AUD <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(ACC_Mid_imp_AUD)

# exploratory model: Med*sex interaction
ACC_Mid_imp_AUD_sex <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(ACC_Mid_imp_AUD_sex)

#------------------#
#   Amygdala (L)
#------------------#

# primary model
Amygdala_L_imp_AUD <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Amygdala_L_imp_AUD)

# exploratory model: Med*sex interaction
Amygdala_L_imp_AUD_sex <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Amygdala_L_imp_AUD_sex)

#------------------#
#   Amygdala (R)
#------------------#

# primary model
Amygdala_R_imp_AUD <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Amygdala_R_imp_AUD)

# exploratory model: Med*sex interaction
Amygdala_R_imp_AUD_sex <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Amygdala_R_imp_AUD_sex)

#------------------#
#   Caudate (L)
#------------------#

# primary model
Caudate_L_imp_AUD <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    sex + dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Caudate_L_imp_AUD)

# exploratory model: Med*sex interaction
Caudate_L_imp_AUD_sex <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Caudate_L_imp_AUD_sex)

#------------------#
#   Caudate (R)
#------------------#

# primary model
Caudate_R_imp_AUD <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + cannabis_days + 
    sex + dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Caudate_R_imp_AUD)

# exploratory model: Med*sex interaction
Caudate_R_imp_AUD_sex <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + cannabis_days +
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Caudate_R_imp_AUD_sex)

#------------------#
#   Insula (L)
#------------------#

# primary model
Insula_L_imp_AUD <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Insula_L_imp_AUD)

# exploratory model: Med*sex interaction
Insula_L_imp_AUD_sex <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + Insula_L_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Insula_L_imp_AUD_sex)

#------------------#
#   Insula (R)
#------------------#

# primary model
Insula_R_imp_AUD <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Insula_R_imp_AUD)

# exploratory model: Med*sex interaction
Insula_R_imp_AUD_sex <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + cannabis_days +
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Insula_R_imp_AUD_sex)

#----------------------#
# Nucleus Accumbens (L)
#----------------------#

# primary model
Nacc_L_imp_AUD <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Nacc_L_imp_AUD)

# exploratory model: Med*sex interaction
Nacc_L_imp_AUD_sex <- lmer(
  Nacc_L ~ Med*sex + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Nacc_L_imp_AUD_sex)

#----------------------#
# Nucleus Accumbens (R)
#----------------------#

# primary model
Nacc_R_imp_AUD <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Nacc_R_imp_AUD)

# exploratory model: Med*sex interaction
Nacc_R_imp_AUD_sex <- lmer(
  Nacc_R ~ Med*sex + Visit + Seq + Nacc_R_Baseline + cannabis_days + 
    drinking_days + (1 | record_id),
  df_imp_2l_AUD)
summary(Nacc_R_imp_AUD_sex)

#------------------#
#   Putamen (L)
#------------------#

# primary model
Putamen_L_imp_AUD <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Putamen_L_imp_AUD)

# exploratory model: Med*sex interaction
Putamen_L_imp_AUD_sex <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Putamen_L_imp_AUD_sex)

#------------------#
#   Putamen (R)
#------------------#

# primary model
Putamen_R_imp_AUD <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Putamen_R_imp_AUD)

# exploratory model: Med*sex interaction
Putamen_R_imp_AUD_sex <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + cannabis_days + 
    dpdd + (1 | record_id),
  df_imp_2l_AUD)
summary(Putamen_R_imp_AUD_sex)

#------------------------------------------------------------------------------#
#                           Aim #2: Craving (self-report)
#------------------------------------------------------------------------------#

#------------------#
#      AOCDS
#------------------#

# primary model
AOCDS_imp_AUD <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    sex + drinks_sum + (1 | record_id),
  df_imp_2l_AUD)
summary(AOCDS_imp_AUD)

# exploratory model: Med*sex interaction
AOCDS_imp_AUD_sex <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + cannabis_days + 
    drinks_sum + (1 | record_id),
  df_imp_2l_AUD)
summary(AOCDS_imp_AUD_sex)

#------------------#
#      ACQ
#------------------#

# primary model
ACQ_imp_AUD <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + cannabis_days + 
    age + sex + drinks_sum +(1 | record_id),
  df_imp_2l_AUD)
summary(ACQ_imp_AUD)

# exploratory model: Med*sex interaction
ACQ_imp_AUD_sex <- lmer(
  ACQ ~ Med*sex + Visit + Seq + ACQ_Baseline + cannabis_days + 
    age + drinks_sum +(1 | record_id),
  df_imp_2l_AUD)
summary(ACQ_imp_AUD_sex)

#------------------------------------------------------------------------------#
#                       consolidate output
#------------------------------------------------------------------------------#

# coefficients from primary model
coef_output_AUD <- tidy(ACC_Mid_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACC (Mid)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation') 

amy_L <- tidy(Amygdala_L_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_R <- tidy(Amygdala_R_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_L <- tidy(Caudate_L_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_R <- tidy(Caudate_R_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_L <- tidy(Insula_L_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_R <- tidy(Insula_R_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_L <- tidy(Nacc_L_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_R <- tidy(Nacc_R_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_L <- tidy(Putamen_L_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_R <- tidy(Putamen_R_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

aocds <- tidy(AOCDS_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'AOCDS', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

acq <- tidy(ACQ_imp_AUD) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACQ', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

coef_output_AUD <- rbind(
  coef_output_AUD, amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
  put_L, put_R, aocds, acq) %>% 
  rename(
    Estimate = 'estimate',
    SE = 'std.error',
    t = 'statistic',
    p = 'p.value')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, put_L, put_R, 
   aocds, acq)

# coefficients from exploratory model
coef_sex_output_AUD <- tidy(ACC_Mid_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACC (Mid)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_L <- tidy(Amygdala_L_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

amy_R <- tidy(Amygdala_R_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Amygdala (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_L <- tidy(Caudate_L_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

cau_R <- tidy(Caudate_R_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Caudate (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_L <- tidy(Insula_L_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

ins_R <- tidy(Insula_R_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Insula (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_L <- tidy(Nacc_L_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

nac_R <- tidy(Nacc_R_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Nucleus Accumbens (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_L <- tidy(Putamen_L_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (L)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

put_R <- tidy(Putamen_R_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'Putamen (R)', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

aocds <- tidy(AOCDS_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'AOCDS', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

acq <- tidy(ACQ_imp_AUD_sex) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  add_row(term = 'ACQ', .before = 1) %>% 
  filter(term != 'sd__(Intercept)' & term != 'sd__Observation')

coef_sex_output_AUD <- rbind(
  coef_sex_output_AUD, amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
  put_L, put_R, aocds, acq) %>% 
  rename(
    Estimate = 'estimate',
    SE = 'std.error',
    t = 'statistic',
    p = 'p.value')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, put_L, put_R, 
   aocds, acq)

# effect size from primary model for Med effect
eff_output_AUD <- eta_squared(
  ACC_Mid_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% # retain med effect size for Med effect only
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACC', variable))

amy_L <- eta_squared(
  Amygdala_L_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (L)', variable))

amy_R <- eta_squared(
  Amygdala_R_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (R)', variable))

cau_L <- eta_squared(
  Caudate_L_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (L)', variable))

cau_R <- eta_squared(
  Caudate_R_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (R)', variable))

ins_L <- eta_squared(
  Insula_L_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (L)', variable))

ins_R <- eta_squared(
  Insula_R_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (R)', variable))

nac_L <- eta_squared(
  Nacc_L_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (L)', variable))

nac_R <- eta_squared(
  Nacc_R_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (R)', variable))

put_L <- eta_squared(
  Putamen_L_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (L)', variable))

put_R <- eta_squared(
  Putamen_R_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (R)', variable))

aocds <- eta_squared(
  AOCDS_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'AOCDS', variable))

acq <- eta_squared(
  ACQ_imp_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACQ', variable))

eff_output_AUD <- rbind(
  eff_output_AUD, amy_L, amy_R, cau_L, cau_R, 
  ins_L, ins_R, nac_L, nac_R, put_L, put_R, aocds, acq) %>% 
  mutate_if(is.numeric, round, 2) 

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
   put_L, put_R, aocds, acq)

# export results
write.csv(coef_output_AUD,'output/coef_output_AUD.csv')
write.csv(coef_sex_output_AUD,'output/coef_sex_output_AUD.csv')
write.csv(eff_output_AUD,'output/eff_output_AUD.csv')

rm(coef_output_AUD, coef_sex_output_AUD, eff_output_AUD)


# Subgoup - Figures  ------------------------------------------------------------

# df split by med and sex 
df_fig_bar_AUD <- df_imp_2l_AUD %>% 
  group_by(Med) %>% 
  summarise(
    mean_Amygdala_L = mean(Amygdala_L),
    sd_Amygdala_L = sd(Amygdala_L),
    mean_Amygdala_R = mean(Amygdala_R),
    sd_Amygdala_R = sd(Amygdala_R))

# rename med
df_fig_bar_AUD <- df_fig_bar_AUD %>% 
  mutate(
    Med = case_when(
      Med == 'NAC' ~ 'N-acetylcysteine',
      Med == 'Placebo' ~ 'Placebo'))

# parameters for figure
limits_L <- aes(ymin=0.01, ymax = mean_Amygdala_L + sd_Amygdala_L)
limits_R <- aes(ymin=0.01, ymax = mean_Amygdala_R + sd_Amygdala_R)
dodge <- position_dodge(width = 0.9)

# ROI: amygdala (L)
ggplot(df_fig_bar_AUD, aes(fill = Med, y = mean_Amygdala_L, x = Med)) +
  geom_errorbar(limits_L, position = dodge, width = 0.25) +
  geom_bar(position = dodge, stat = "identity") +
  scale_fill_manual(values = c("#05C3DE", "#054C70")) +
  theme_classic(base_size = 18) +
  labs(fill = "Medication") +
  labs(x ="Medication", y = "BOLD Signal (Alcohol - Neutral)", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial"))

# ROI: amygdala (R)
ggplot(df_fig_bar_AUD, aes(fill = Med, y = mean_Amygdala_R, x = Med)) +
  geom_errorbar(limits_R, position = dodge, width = 0.25) +
  geom_bar(position = dodge, stat = "identity") +
  scale_fill_manual(values = c("#05C3DE", "#054C70")) +
  theme_classic(base_size = 18) +
  labs(fill = "Medication") +
  labs(x ="Medication", y = "BOLD Signal (Alcohol - Neutral)", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial"))

rm(df_fig_bar_AUD, limits_L, limits_R, dodge)




