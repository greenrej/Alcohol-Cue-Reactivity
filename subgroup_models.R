
# Analyses among subgroup that met criteria for AUD

# Outcome - ROIs ---------------------------------------------------------------

# exploratory models: medication x sex interaction

#------------------------------------------------------------------------------#
#                                 ACC - Mid
#------------------------------------------------------------------------------#

# (1) primary model 

ACC_1_AUD <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(ACC_1_AUD) 
check_singularity(ACC_1_AUD) 
check_outliers(ACC_1_AUD)
vif(ACC_1_AUD) 
check_model(ACC_1_AUD, check = 'all') 
check_heteroscedasticity(ACC_1_AUD) 

# - save model object
ACC_1_AUD_summary <- broom::tidy(ACC_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 
ACC_2_AUD <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(ACC_2_AUD) 
check_singularity(ACC_2_AUD) 
check_outliers(ACC_2_AUD) 
vif(ACC_2_AUD) 
check_model(ACC_2_AUD, check = 'all')
check_heteroscedasticity(ACC_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(ACC_2_AUD)
summary(temp)
rm(temp)

# - save model object (interaction effect only)
ACC_2_AUD_summary <- broom::tidy(ACC_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'ACC_Mid', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(ACC_1_AUD_summary, 
          'output/models_primary/AUD/ACC_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- ACC_2_AUD_summary 

rm(ACC_1_AUD_summary, ACC_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Amygdala - L
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_L_1_AUD <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + dpdd_base +
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Amy_L_1_AUD) 
check_singularity(Amy_L_1_AUD) 
check_outliers(Amy_L_1_AUD)
vif(Amy_L_1_AUD)
check_model(Amy_L_1_AUD, check = 'all') 
check_heteroscedasticity(Amy_L_1_AUD) 

# - save model object
Amy_L_1_AUD_summary <- broom::tidy(Amy_L_1_AUD) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Amy_L_2_AUD <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Amy_L_2_AUD) 
check_singularity(Amy_L_2_AUD) 
check_outliers(Amy_L_2_AUD) 
vif(Amy_L_2_AUD) 
check_model(Amy_L_2_AUD, check = 'all') 
check_heteroscedasticity(Amy_L_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Amy_L_2_AUD)
summary(temp)
rm(temp)

# - save model object
Amy_L_2_AUD_summary <- broom::tidy(Amy_L_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Amy_L_1_AUD_summary, 
          'output/models_primary/AUD/Amy_L_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_L_2_AUD_summary) 

rm(Amy_L_1_AUD_summary, Amy_L_2_AUD_summary)

# follow-up: 

# df split by med
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

#------------------------------------------------------------------------------#
#                               Amygdala - R
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_R_1_AUD <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Amy_R_1_AUD) 
check_singularity(Amy_R_1_AUD) 
check_outliers(Amy_R_1_AUD) 
vif(Amy_R_1_AUD) 
check_model(Amy_R_1_AUD, check = 'all') 
check_heteroscedasticity(Amy_R_1_AUD) 

# - model follow-up: singuarlity
#   - drop dpdd 
#   - drop random ID
temp_1 <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)
check_singularity(temp_1)

temp_2 <- lm(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + 
    cannabis_days_base + nicotine_days_base,
  df_imp_2l_AUD)
check_singularity(temp_2)

summary(Amy_R_1_AUD)
summary(temp_1)
summary(temp_2)
rm(temp_1, temp_2)

# - save model object 
Amy_R_1_AUD_summary <- broom::tidy(Amy_R_1_AUD) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model

Amy_R_2_AUD <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Amy_R_2_AUD) 
check_singularity(Amy_R_2_AUD) 
check_outliers(Amy_R_2_AUD) 
vif(Amy_R_2_AUD)
check_model(Amy_R_2_AUD, check = 'all') 
check_heteroscedasticity(Amy_R_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Amy_R_2_AUD)
summary(temp)
rm(temp)

# - save model object
Amy_R_2_AUD_summary <- broom::tidy(Amy_R_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Amy_R_1_AUD_summary, 
          'output/models_primary/AUD/Amy_R_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_R_2_AUD_summary) 

rm(Amy_R_1_AUD_summary, Amy_R_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Caudate - L
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_L_1_AUD <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Cau_L_1_AUD) 
check_singularity(Cau_L_1_AUD) 
check_outliers(Cau_L_1_AUD) 
vif(Cau_L_1_AUD) 
check_model(Cau_L_1_AUD, check = 'all') 
check_heteroscedasticity(Cau_L_1_AUD) 

# - model follow-up: drop dpdd and sex
temp <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)
summary(Cau_L_1_AUD)
summary(temp)
rm(temp)

# - save model object
Cau_L_1_AUD_summary <- broom::tidy(Cau_L_1_AUD) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_L_2_AUD <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Cau_L_2_AUD) 
check_singularity(Cau_L_2_AUD) 
check_outliers(Cau_L_2_AUD) 
vif(Cau_L_2_AUD) 
check_model(Cau_L_2_AUD, check = 'all') 
check_heteroscedasticity(Cau_L_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Cau_L_2_AUD)
summary(temp)
rm(temp)

# - save model object
Cau_L_2_AUD_summary <- broom::tidy(Cau_L_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Cau_L_1_AUD_summary, 
          'output/models_primary/AUD/Cau_L_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_L_2_AUD_summary) 

rm(Cau_L_1_AUD_summary, Cau_L_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Caudate - R
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_R_1_AUD <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + sex +
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Cau_R_1_AUD) 
check_singularity(Cau_R_1_AUD) 
check_outliers(Cau_R_1_AUD) 
vif(Cau_R_1_AUD) 
check_model(Cau_R_1_AUD, check = 'all') 
check_heteroscedasticity(Cau_R_1_AUD) 

# - model follow-up: 
#   - drop dpdd and sex
#   - drop random effect
temp_1 <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + 
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

temp_2 <- lm(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + dpdd_base +
    cannabis_days_base + nicotine_days_base + sex,
  df_imp_2l_AUD)

summary(Cau_R_1_AUD)
summary(temp_1)
summary(temp_2)
rm(temp_1, temp_2)

# - save model object
Cau_R_1_AUD_summary <- broom::tidy(Cau_R_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_R_2_AUD <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Cau_R_2_AUD) 
check_singularity(Cau_R_2_AUD) 
check_outliers(Cau_R_2_AUD)
vif(Cau_R_2_AUD) 
check_model(Cau_R_2_AUD, check = 'all') 
check_heteroscedasticity(Cau_R_2_AUD) 

# - model follow-up: 
#   - drop dpdd
#   - drop random effect
temp_1 <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + 
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)

temp_2 <- lm(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + dpdd_base +
    cannabis_days_base + nicotine_days_base + sex,
  df_imp_2l_AUD)

summary(Cau_R_1_AUD)
summary(temp_1)
summary(temp_2)
rm(temp_1, temp_2)

# - save model object
Cau_R_2_AUD_summary <- broom::tidy(Cau_R_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Cau_R_1_AUD_summary, 
          'output/models_primary/AUD/Cau_R_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_R_2_AUD_summary) 

rm(Cau_R_1_AUD_summary, Cau_R_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Insula - L
#------------------------------------------------------------------------------#

# (1) primary model 

Ins_L_1_AUD <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Ins_L_1_AUD) 
check_singularity(Ins_L_1_AUD)
check_outliers(Ins_L_1_AUD) 
vif(Ins_L_1_AUD) 
check_model(Ins_L_1_AUD, check = 'all')
check_heteroscedasticity(Ins_L_1_AUD)

# - model follow-up: drop baseline cue-reactivity and nicotine use days
temp <- lmer(
  Insula_L ~ Med + Visit + Seq + dpdd_base + cannabis_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)
summary(Ins_L_1_AUD)
summary(temp)
rm(temp)

# - save model object
Ins_L_1_AUD_summary <- broom::tidy(Ins_L_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_L_2_AUD <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + Insula_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Ins_L_2_AUD) 
check_singularity(Ins_L_2_AUD) 
check_outliers(Ins_L_2_AUD) 
vif(Ins_L_2_AUD) 
check_model(Ins_L_2_AUD, check = 'all') 
check_heteroscedasticity(Ins_L_2_AUD) 

# - model follow-up: 
temp <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Ins_L_2_AUD)
summary(temp)
rm(temp)

# - save model object
Ins_L_2_AUD_summary <- broom::tidy(Ins_L_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_L', NA)) %>% 
  relocate(model)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_L_2_AUD_summary) 

# (4) export relevant output
write.csv(Ins_L_1_AUD_summary, 
          'output/models_primary/AUD/Ins_L_1_AUD_summary.csv')

rm(Ins_L_1_AUD_summary, Ins_L_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Insula - R
#------------------------------------------------------------------------------#

# (1) primary model 
Ins_R_1_AUD <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Ins_R_1_AUD) 
check_singularity(Ins_R_1_AUD) 
check_outliers(Ins_R_1_AUD) 
vif(Ins_R_1_AUD) 
check_model(Ins_R_1_AUD, check = 'all') 
check_heteroscedasticity(Ins_R_1_AUD) 

# - save model object
Ins_R_1_AUD_summary <- broom::tidy(Ins_R_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_R_2_AUD <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Ins_R_2_AUD) 
check_singularity(Ins_R_2_AUD) 
check_outliers(Ins_R_2_AUD) 
vif(Ins_R_2_AUD) 
check_model(Ins_R_2_AUD, check = 'all') 
check_heteroscedasticity(Ins_R_2_AUD) 

# - model follow-up: drop nicotine days
temp <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + drinking_days_base + 
    cannabis_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Ins_R_2_AUD)
summary(temp)
rm(temp)

# - save model object
Ins_R_2_AUD_summary <- broom::tidy(Ins_R_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Ins_R_1_AUD_summary, 
          'output/models_primary/AUD/Ins_R_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_R_2_AUD_summary) 

rm(Ins_R_1_AUD_summary, Ins_R_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Nacc - L
#------------------------------------------------------------------------------#

# (1) primary model 

Nac_L_1_AUD <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Nac_L_1_AUD) 
check_singularity(Nac_L_1_AUD) 
check_outliers(Nac_L_1_AUD) 
vif(Nac_L_1_AUD) 
check_model(Nac_L_1_AUD, check = 'all') 
check_heteroscedasticity(Nac_L_1_AUD) 

# - save model object
Nac_L_1_AUD_summary <- broom::tidy(Nac_L_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_L_2_AUD <- lmer(
  Nacc_L ~ Med*sex + Visit + Seq + Nacc_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Nac_L_2_AUD) 
check_singularity(Nac_L_2_AUD) 
check_outliers(Nac_L_2_AUD) 
vif(Nac_L_2_AUD) 
check_model(Nac_L_2_AUD, check = 'all') 
check_heteroscedasticity(Nac_L_2_AUD) 

# - save model object
Nac_L_2_AUD_summary <- broom::tidy(Nac_L_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_L_1_AUD_summary, 
          'output/models_primary/AUD/Nac_L_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_L_2_AUD_summary) 

rm(Nac_L_1_AUD_summary, Nac_L_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Nacc - R
#------------------------------------------------------------------------------#

# (1) primary model 

Nac_R_1_AUD <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Nac_R_1_AUD) 
check_singularity(Nac_R_1_AUD) 
check_outliers(Nac_R_1_AUD) 
vif(Nac_R_1_AUD)
check_model(Nac_R_1_AUD, check = 'all') 
check_heteroscedasticity(Nac_R_1_AUD) 

# - save model object
Nac_R_1_AUD_summary <- broom::tidy(Nac_R_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_R_2_AUD <- lmer(
  Nacc_R ~ Med*sex + Visit + Seq + Nacc_R_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Nac_R_2_AUD) 
check_singularity(Nac_R_2_AUD) 
check_outliers(Nac_R_2_AUD) 
vif(Nac_R_2_AUD) 
check_model(Nac_R_2_AUD, check = 'all') 
check_heteroscedasticity(Nac_R_2_AUD)

# - save model object
Nac_R_2_AUD_summary <- broom::tidy(Nac_R_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_R_1_AUD_summary, 
          'output/models_primary/AUD/Nac_R_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_R_2_AUD_summary) 

rm(Nac_R_1_AUD_summary, Nac_R_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Putamen - L
#------------------------------------------------------------------------------#

# (1) primary model

Put_L_1_AUD <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Put_L_1_AUD) 
check_singularity(Put_L_1_AUD) 
check_outliers(Put_L_1_AUD) 
vif(Put_L_1_AUD) 
check_model(Put_L_1_AUD, check = 'all') 
check_heteroscedasticity(Put_L_1_AUD) 

# - save model object
Put_L_1_AUD_summary <- broom::tidy(Put_L_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_L_2_AUD <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Put_L_2_AUD)
check_singularity(Put_L_2_AUD) 
check_outliers(Put_L_2_AUD) 
vif(Put_L_2_AUD) 
check_model(Put_L_2_AUD, check = 'all') 
check_heteroscedasticity(Put_L_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Put_L_2_AUD)
summary(temp)
rm(temp)

# - save model object
Put_L_2_AUD_summary <- broom::tidy(Put_L_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_L_1_AUD_summary, 
          'output/models_primary/AUD/Put_L_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_L_2_AUD_summary) 

rm(Put_L_1_AUD_summary, Put_L_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               Putamen - R
#------------------------------------------------------------------------------#

# (1) primary model 

Put_R_1_AUD <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(Put_R_1_AUD) 
check_singularity(Put_R_1_AUD) 
check_outliers(Put_R_1_AUD) 
vif(Put_R_1_AUD) 
check_model(Put_R_1_AUD, check = 'all') 
check_heteroscedasticity(Put_R_1_AUD) 

# - save model object
Put_R_1_AUD_summary <- broom::tidy(Put_R_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_R_2_AUD <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 

# - model performance check
testDispersion(Put_R_2_AUD) 
check_singularity(Put_R_2_AUD) 
check_outliers(Put_R_2_AUD)
vif(Put_R_2_AUD) 
check_model(Put_R_2_AUD, check = 'all') 
check_heteroscedasticity(Put_R_2_AUD) 

# - model follow-up: drop dpdd
temp <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD) 
summary(Put_R_2_AUD)
summary(temp)
rm(temp)

# - save model object
Put_R_2_AUD_summary <- broom::tidy(Put_R_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_R_1_AUD_summary, 
          'output/models_primary/AUD/Put_R_1_AUD_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_R_2_AUD_summary) 

rm(Put_R_1_AUD_summary, Put_R_2_AUD_summary)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output <- effectsize::eta_squared(
  ACC_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% # retain med effect size for Med effect only
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACC', variable))

amy_L <- effectsize::eta_squared(
  Amy_L_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (L)', variable))

amy_R <- effectsize::eta_squared(
  Amy_R_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (R)', variable))

cau_L <- effectsize::eta_squared(
  Cau_L_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (L)', variable))

cau_R <- effectsize::eta_squared(
  Cau_R_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (R)', variable))

ins_L <- effectsize::eta_squared(
  Ins_L_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (L)', variable))

ins_R <- effectsize::eta_squared(
  Ins_R_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (R)', variable))

nac_L <- effectsize::eta_squared(
  Nac_L_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (L)', variable))

nac_R <- effectsize::eta_squared(
  Nac_R_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (R)', variable))

put_L <- effectsize::eta_squared(
  Put_L_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (L)', variable))

put_R <- effectsize::eta_squared(
  Put_R_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (R)', variable))

eff_output <- rbind(eff_output, amy_L, amy_R, cau_L, cau_R, 
                    ins_L, ins_R, nac_L, nac_R, put_L, put_R) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output,'output/models_primary/AUD/eff_output.csv')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
   put_L, put_R, eff_output)

### Export med*sex results
write.csv(exp_int, 'output/models_exploratory/AUD/exp_int.csv')

rm(exp_int)


# Outcome - Subjective Craving -------------------------------------------------

# exploratory models: medication x sex interaction

#------------------------------------------------------------------------------#
#                                 AOCDS
#------------------------------------------------------------------------------#

# (1) primary model 

AOC_1_AUD <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + drinks_sum_base + 
    cannabis_days_base + nicotine_days_base + sex +
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(AOC_1_AUD) 
check_singularity(AOC_1_AUD) 
check_outliers(AOC_1_AUD) 
vif(AOC_1_AUD) 
check_model(AOC_1_AUD, check = 'all') 
check_heteroscedasticity(AOC_1_AUD) 

# - save model object
AOC_1_AUD_summary <- broom::tidy(AOC_1_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

AOC_2_AUD <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + drinks_sum_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(AOC_2_AUD) 
check_singularity(AOC_2_AUD) 
check_outliers(AOC_2_AUD)
vif(AOC_2_AUD) 
check_model(AOC_2_AUD, check = 'all') 
check_heteroscedasticity(AOC_2_AUD) 

# - save model object (interaction effect only)
AOC_2_AUD_summary <- broom::tidy(AOC_2_AUD) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'AOCDS', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(AOC_1_AUD_summary, 
          'output/models_primary/AUD/AOC_1_AUD_summary.csv')
write.csv(AOC_2_AUD_summary, 
          'output/models_exploratory/AUD/AOC_2_AUD_summary.csv')

rm(AOC_1_AUD_summary, AOC_2_AUD_summary)

#------------------------------------------------------------------------------#
#                               ACQ
#------------------------------------------------------------------------------#

# (1) primary model 

ACQ_1_AUD <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + drinks_sum_base +
    cannabis_days_base + nicotine_days_base + sex + age +
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(ACQ_1_AUD) 
check_singularity(ACQ_1_AUD) 
check_outliers(ACQ_1_AUD) 
vif(ACQ_1_AUD) 
check_model(ACQ_1_AUD, check = 'all')
check_heteroscedasticity(ACQ_1_AUD) 

# - model follow-up: 
#   - drop baseline, sex, and age
#   - robust SE
temp <- lmer(
  ACQ ~ Med + Visit + Seq + drinks_sum_base + cannabis_days_base + 
    nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)
summary(ACQ_1_AUD)
summary(temp)
rm(temp)

ACQ_1_AUD_robust <- robust_mixed(ACQ_1_AUD, type = 'CR2')
ACQ_1_AUD_robust
summary(ACQ_1_AUD)

# - save model object (w/robust SE and covariates)
ACQ_1_AUD_robust_summary <- broom::tidy(ACQ_1_AUD_robust) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-mb.se, -stars) %>% 
  rename(df = statistic)

# (2) exploratory model 

ACQ_2_AUD <- lmer(
  ACQ ~ Med*sex + Visit + Seq + ACQ_Baseline + drinks_sum_base +
    cannabis_days_base + nicotine_days_base + age +
    (1 | record_id),
  df_imp_2l_AUD)

# - model performance check
testDispersion(ACQ_2_AUD) 
check_singularity(ACQ_2_AUD)
check_outliers(ACQ_2_AUD) 
vif(ACQ_2_AUD) 
check_model(ACQ_2_AUD, check = 'all') 
check_heteroscedasticity(ACQ_2_AUD)

# - model follow-up: 
#   - drop ACQ baseline and age
#   - robust SE
temp <- lmer(
  ACQ ~ Med*sex + Visit + Seq + drinks_sum_base +
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l_AUD)
summary(ACQ_2_AUD)
summary(temp)
rm(temp)

ACQ_2_AUD_robust <- robust_mixed(ACQ_2_AUD, type = 'CR2')
ACQ_2_AUD_robust
summary(ACQ_2_AUD)

# - save model object
ACQ_2_AUD_robust_summary <- broom::tidy(ACQ_2_AUD_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -stars) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  mutate(model = 'ACQ', 
         effect = 'fixed') %>% 
  relocate(model, effect, everything()) %>% 
  rename(df = statistic,
         statistic = t.stat)

# (4) export relevant output
write.csv(ACQ_1_AUD_robust_summary, 
          'output/models_primary/AUD/ACQ_1_AUD_robust_summary.csv')
write.csv(ACQ_2_AUD_robust_summary, 
          'output/models_exploratory/AUD/ACQ_2_AUD_robust_summary.csv')

rm(ACQ_1_AUD_robust, ACQ_2_AUD_robust,
   ACQ_1_AUD_robust_summary, ACQ_2_AUD_robust_summary)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output_sr <- effectsize::eta_squared(
  AOC_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'AOCDS', variable))

acq <- effectsize::eta_squared(
  ACQ_1_AUD, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACQ', variable))

eff_output_sr <- rbind(eff_output_sr, acq) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output_sr,'output/models_primary/AUD/eff_output_sr.csv')

rm(eff_output_sr, acq,
   AOC_1_AUD, AOC_2_AUD, ACQ_1_AUD, ACQ_2_AUD)



