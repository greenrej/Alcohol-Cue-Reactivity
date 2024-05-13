# Whole Sample: Outcome - ROIs -------------------------------------------

# exploratory models: medication x sex interaction

#------------------------------------------------------------------------------#
#                                 ACC - Mid
#------------------------------------------------------------------------------#

# (1) primary model 

ACC_1_imp <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(ACC_1_imp) 
check_singularity(ACC_1_imp) 
check_outliers(ACC_1_imp) 
ACC_1_imp_vif <- vif(ACC_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
ACC_1_imp_vif 
check_model(ACC_1_imp, check = 'all') 
check_heteroscedasticity(ACC_1_imp)

# - save model object
ACC_1_imp_summary <- broom::tidy(ACC_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

ACC_2_imp <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(ACC_2_imp) 
check_singularity(ACC_2_imp) 
check_outliers(ACC_2_imp) 
vif(ACC_2_imp) 
check_model(ACC_2_imp, check = 'all') 
check_heteroscedasticity(ACC_2_imp) 

# - save model object (interaction effect only)
ACC_2_imp_summary <- broom::tidy(ACC_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'ACC_Mid', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(ACC_1_imp_summary, 
          'output/models_primary/ACC_1_imp_summary.csv')
write.csv(ACC_1_imp_vif, 
          'output/models_primary/ACC_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- ACC_2_imp_summary 
exp_int_boot <- ACC_3b_imp_summary %>% 
  mutate(model = 'ACC_Mid') %>% 
  relocate(model, everything())

rm(ACC_1_imp_summary, ACC_2_imp_summary, ACC_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Amygdala - L
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_L_1_imp <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + 
    mini10_dependence_a_rec + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Amy_L_1_imp)
check_singularity(Amy_L_1_imp) 
check_outliers(Amy_L_1_imp) 
Amy_L_1_imp_vif <- vif(Amy_L_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Amy_L_1_imp_vif 
check_model(Amy_L_1_imp, check = 'all') 
check_heteroscedasticity(Amy_L_1_imp)

# - save model object
Amy_L_1_imp_summary <- broom::tidy(Amy_L_1_imp) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Amy_L_2_imp <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + 
    mini10_dependence_a_rec + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Amy_L_2_imp) 
check_singularity(Amy_L_2_imp) 
check_outliers(Amy_L_2_imp)
vif(Amy_L_2_imp) 
check_model(Amy_L_2_imp, check = 'all') 
check_heteroscedasticity(Amy_L_2_imp) 

# - save model object
Amy_L_2_imp_summary <- broom::tidy(Amy_L_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Amy_L_1_imp_summary, 
          'output/models_primary/Amy_L_1_imp_summary.csv')
write.csv(Amy_L_1_imp_vif, 
          'output/models_primary/Amy_L_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_L_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Amy_L_3b_imp_summary) 

rm(Amy_L_1_imp_summary, Amy_L_2_imp_summary, Amy_L_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Amygdala - R
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_R_1_imp <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Amy_R_1_imp) 
check_singularity(Amy_R_1_imp) 
check_outliers(Amy_R_1_imp) 
Amy_R_1_imp_vif <- vif(Amy_R_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Amy_R_1_imp_vif 
check_model(Amy_R_1_imp, check = 'all') 
check_heteroscedasticity(Amy_R_1_imp) 

# - save model object
Amy_R_1_imp_summary <- broom::tidy(Amy_R_1_imp) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Amy_R_2_imp <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Amy_R_2_imp) 
check_singularity(Amy_R_2_imp) 
check_outliers(Amy_R_2_imp) 
Amy_R_2_imp_vif <- vif(Amy_R_2_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Amy_R_2_imp_vif
check_model(Amy_R_2_imp, check = 'all') 
check_heteroscedasticity(Amy_R_2_imp)

# - save model object
Amy_R_2_imp_summary <- broom::tidy(Amy_R_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Amy_R_1_imp_summary, 
          'output/models_primary/Amy_R_1_imp_summary.csv')
write.csv(Amy_R_1_imp_vif, 
          'output/models_primary/Amy_R_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_R_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Amy_R_3b_imp_summary) 

rm(Amy_R_1_imp_summary, Amy_R_2_imp_summary, Amy_R_1_imp_vif, Amy_R_2_imp_vif)

#------------------------------------------------------------------------------#
#                               Caudate - L
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_L_1_imp <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Cau_L_1_imp) 
check_singularity(Cau_L_1_imp)
check_outliers(Cau_L_1_imp) 
Cau_L_1_imp_vif <- vif(Cau_L_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Cau_L_1_imp_vif 
check_model(Cau_L_1_imp, check = 'all') 
check_heteroscedasticity(Cau_L_1_imp) 

# - save model object
Cau_L_1_imp_summary <- broom::tidy(Cau_L_1_imp) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_L_2_imp <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Cau_L_2_imp) 
check_singularity(Cau_L_2_imp) 
check_outliers(Cau_L_2_imp) 
vif(Cau_L_2_imp) 
check_model(Cau_L_2_imp, check = 'all') 
check_heteroscedasticity(Cau_L_2_imp)

# - save model object
Cau_L_2_imp_summary <- broom::tidy(Cau_L_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Cau_L_1_imp_summary, 
          'output/models_primary/Cau_L_1_imp_summary.csv')
write.csv(Cau_L_1_imp_vif, 
          'output/models_primary/Cau_L_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_L_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Cau_L_3b_imp_summary) 

rm(Cau_L_1_imp_summary, Cau_L_2_imp_summary, Cau_L_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Caudate - R
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_R_1_imp <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Cau_R_1_imp) 
check_singularity(Cau_R_1_imp) 
check_outliers(Cau_R_1_imp) 
Cau_R_1_imp_vif <- vif(Cau_R_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Cau_R_1_imp_vif 
check_model(Cau_R_1_imp, check = 'all') 
check_heteroscedasticity(Cau_R_1_imp)

# - save model object
Cau_R_1_imp_summary <- broom::tidy(Cau_R_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_R_2_imp <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Cau_R_2_imp)
check_singularity(Cau_R_2_imp)
check_outliers(Cau_R_2_imp) 
vif(Cau_R_2_imp) 
check_model(Cau_R_2_imp, check = 'all') 
check_heteroscedasticity(Cau_R_2_imp) 

# - save model object
Cau_R_2_imp_summary <- broom::tidy(Cau_R_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Cau_R_1_imp_summary, 
          'output/models_primary/Cau_R_1_imp_summary.csv')
write.csv(Cau_R_1_imp_vif, 
          'output/models_primary/Cau_R_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_R_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Cau_R_3b_imp_summary) 

rm(Cau_R_1_imp_summary, Cau_R_2_imp_summary, Cau_R_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Insula - L
#------------------------------------------------------------------------------#

# (1) primary model 

Ins_L_1_imp <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Ins_L_1_imp) 
check_singularity(Ins_L_1_imp) 
check_outliers(Ins_L_1_imp) 
Ins_L_1_imp_vif <- vif(Ins_L_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Ins_L_1_imp_vif 
check_model(Ins_L_1_imp, check = 'all') 
check_heteroscedasticity(Ins_L_1_imp) 

# - save model object
Ins_L_1_imp_summary <- broom::tidy(Ins_L_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_L_2_imp <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + Insula_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Ins_L_2_imp) 
check_singularity(Ins_L_2_imp) 
check_outliers(Ins_L_2_imp) 
vif(Ins_L_2_imp) 
check_model(Ins_L_2_imp, check = 'all') 
check_heteroscedasticity(Ins_L_2_imp) 

# - save model object
Ins_L_2_imp_summary <- broom::tidy(Ins_L_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_L', NA)) %>% 
  relocate(model)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_L_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Ins_L_3b_imp_summary) 

# (4) export relevant output
write.csv(Ins_L_1_imp_summary, 
          'output/models_primary/Ins_L_1_imp_summary.csv')
write.csv(Ins_L_1_imp_vif, 
          'output/models_primary/Ins_L_1_imp_vif.csv', row.names = TRUE)

rm(Ins_L_1_imp_summary, Ins_L_2_imp_summary, Ins_L_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Insula - R
#------------------------------------------------------------------------------#

# (1) primary model 

Ins_R_1_imp <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Ins_R_1_imp) 
check_singularity(Ins_R_1_imp) 
check_outliers(Ins_R_1_imp) 
Ins_R_1_imp_vif <- vif(Ins_R_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Ins_R_1_imp_vif 
check_model(Ins_R_1_imp, check = 'all') 
check_heteroscedasticity(Ins_R_1_imp) 

# - save model object
Ins_R_1_imp_summary <- broom::tidy(Ins_R_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_R_2_imp <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Ins_R_2_imp) 
check_singularity(Ins_R_2_imp) 
check_outliers(Ins_R_2_imp) 
vif(Ins_R_2_imp) 
check_model(Ins_R_2_imp, check = 'all') 
check_heteroscedasticity(Ins_R_2_imp) 

# - save model object
Ins_R_2_imp_summary <- broom::tidy(Ins_R_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Ins_R_1_imp_summary, 
          'output/models_primary/Ins_R_1_imp_summary.csv')
write.csv(Ins_R_1_imp_vif, 
          'output/models_primary/Ins_R_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_R_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Ins_R_3b_imp_summary) 

rm(Ins_R_1_imp_summary, Ins_R_2_imp_summary, Ins_R_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Nacc - L
#------------------------------------------------------------------------------#

# (1) primary model 

Nac_L_1_imp <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + mini10_dependence_a_rec + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Nac_L_1_imp) 
check_singularity(Nac_L_1_imp)
check_outliers(Nac_L_1_imp) 
Nac_L_1_imp_vif <- vif(Nac_L_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Nac_L_1_imp_vif 
check_model(Nac_L_1_imp, check = 'all') 
check_heteroscedasticity(Nac_L_1_imp) 

# - save model object
Nac_L_1_imp_summary <- broom::tidy(Nac_L_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_L_2_imp <- lmer(
  Nacc_L ~ Med*sex + Visit + Seq + Nacc_L_Baseline + mini10_dependence_a_rec + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Nac_L_2_imp)
check_singularity(Nac_L_2_imp) 
check_outliers(Nac_L_2_imp) 
vif(Nac_L_2_imp) 
check_model(Nac_L_2_imp, check = 'all')
check_heteroscedasticity(Nac_L_2_imp) 

# - save model object
Nac_L_2_imp_summary <- broom::tidy(Nac_L_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_L_1_imp_summary, 
          'output/models_primary/Nac_L_1_imp_summary.csv')
write.csv(Nac_L_1_imp_vif,
          'output/models_primary/Nac_L_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_L_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Nac_L_3b_imp_summary) 

rm(Nac_L_1_imp_summary, Nac_L_2_imp_summary, Nac_L_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Nacc - R
#------------------------------------------------------------------------------#

# (1) primary model 

Nac_R_1_imp <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + mini10_dependence + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Nac_R_1_imp) 
check_singularity(Nac_R_1_imp) 
check_outliers(Nac_R_1_imp)
Nac_R_1_imp_vif <- vif(Nac_R_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Nac_R_1_imp_vif 
check_model(Nac_R_1_imp, check = 'all') 
check_heteroscedasticity(Nac_R_1_imp)

# - save model object
Nac_R_1_imp_summary <- broom::tidy(Nac_R_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_R_2_imp <- lmer(
  Nacc_R ~ Med*sex + Visit + Seq + Nacc_R_Baseline + mini10_dependence + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Nac_R_2_imp) 
check_singularity(Nac_R_2_imp) 
check_outliers(Nac_R_2_imp) 
vif(Nac_R_2_imp) 
check_model(Nac_R_2_imp, check = 'all') 
check_heteroscedasticity(Nac_R_2_imp)

# - save model object
Nac_R_2_imp_summary <- broom::tidy(Nac_R_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_R_1_imp_summary, 
          'output/models_primary/Nac_R_1_imp_summary.csv')
write.csv(Nac_R_1_imp_vif, 
          'output/models_primary/Nac_R_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_R_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Nac_R_3b_imp_summary) 

rm(Nac_R_1_imp_summary, Nac_R_2_imp_summary, Nac_R_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Putamen - L
#------------------------------------------------------------------------------#

# (1) primary model 

Put_L_1_imp <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Put_L_1_imp) 
check_singularity(Put_L_1_imp) 
check_outliers(Put_L_1_imp) 
Put_L_1_imp_vif <- vif(Put_L_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Put_L_1_imp_vif
check_model(Put_L_1_imp, check = 'all')
check_heteroscedasticity(Put_L_1_imp) 

# - save model object
Put_L_1_imp_summary <- broom::tidy(Put_L_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_L_2_imp <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Put_L_2_imp) 
check_singularity(Put_L_2_imp) 
check_outliers(Put_L_2_imp) 
vif(Put_L_2_imp) 
check_model(Put_L_2_imp, check = 'all') 
check_heteroscedasticity(Put_L_2_imp)

# - save model object
Put_L_2_imp_summary <- broom::tidy(Put_L_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_L_1_imp_summary, 
          'output/models_primary/Put_L_1_imp_summary.csv')
write.csv(Put_L_1_imp_vif, 'output/models_primary/Put_L_1_imp_vif.csv',
          row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_L_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Put_L_3b_imp_summary) 

rm(Put_L_1_imp_summary, Put_L_2_imp_summary, Put_L_1_imp_vif)

#------------------------------------------------------------------------------#
#                               Putamen - R
#------------------------------------------------------------------------------#

# (1) primary model 

Put_R_1_imp <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + sex + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(Put_R_1_imp) 
check_singularity(Put_R_1_imp) 
check_outliers(Put_R_1_imp) 
Put_R_1_imp_vif <- vif(Put_R_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
Put_R_1_imp_vif
check_model(Put_R_1_imp, check = 'all') 
check_heteroscedasticity(Put_R_1_imp) 

# - save model object
Put_R_1_imp_summary <- broom::tidy(Put_R_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_R_2_imp <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(Put_R_2_imp)
check_singularity(Put_R_2_imp) 
check_outliers(Put_R_2_imp) 
vif(Put_R_2_imp) 
check_model(Put_R_2_imp, check = 'all') 
check_heteroscedasticity(Put_R_2_imp) 

# - save model object
Put_R_2_imp_summary <- broom::tidy(Put_R_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_R_1_imp_summary, 
          'output/models_primary/Put_R_1_imp_summary.csv')
write.csv(Put_R_1_imp_vif, 
          'output/models_primary/Put_R_1_imp_vif.csv', row.names = TRUE)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_R_2_imp_summary) 
exp_int_boot <- rbind(exp_int_boot, Put_R_3b_imp_summary)

rm(Put_R_1_imp_summary, Put_R_2_imp_summary, Put_R_1_imp_vif)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output <- effectsize::eta_squared(
  ACC_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% # retain med effect size for Med effect only
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACC', variable))

amy_L <- effectsize::eta_squared(
  Amy_L_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (L)', variable))

amy_R <- effectsize::eta_squared(
  Amy_R_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (R)', variable))

cau_L <- effectsize::eta_squared(
  Cau_L_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (L)', variable))

cau_R <- effectsize::eta_squared(
  Cau_R_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (R)', variable))

ins_L <- effectsize::eta_squared(
  Ins_L_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (L)', variable))

ins_R <- effectsize::eta_squared(
  Ins_R_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (R)', variable))

nac_L <- effectsize::eta_squared(
  Nac_L_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (L)', variable))

nac_R <- effectsize::eta_squared(
  Nac_R_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (R)', variable))

put_L <- effectsize::eta_squared(
  Put_L_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (L)', variable))

put_R <- effectsize::eta_squared(
  Put_R_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (R)', variable))

eff_output <- rbind(eff_output, amy_L, amy_R, cau_L, cau_R, 
                    ins_L, ins_R, nac_L, nac_R, put_L, put_R) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output,'output/models_primary/eff_output.csv')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
   put_L, put_R, eff_output)

### Export med*sex results
write.csv(exp_int, 'output/models_exploratory/exp_int.csv')
write.csv(exp_int_boot, 'output/models_exploratory/exp_int_boot.csv')

rm(exp_int, exp_int_boot)

# follow-up: effect size for med x sex in amygdala
Amy_L_interation <- effectsize::eta_squared(
  Amy_L_2_imp, partial = TRUE, alternative = 'two.sided') 

Amy_R_interation <- effectsize::eta_squared(
  Amy_R_2_imp, partial = TRUE, alternative = 'two.sided') 


# Whole Sample: ROI Multiple Comparison Correction ---------------------------------------------

# ACC - Mid
ACC_1_imp_p <- summary(ACC_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(ACC_Mid = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'ACC_Mid_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, ACC_Mid) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'ACC_Mid')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
ACC_1_imp_p <- rbind(ACC_1_imp_p, demo) 
rm(col_names, demo) 

ACC_2_imp_p <- summary(ACC_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(ACC_Mid = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, ACC_Mid)  

# Amygdala - L
Amy_L_1_imp_p <- summary(Amy_L_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependence_a_rec', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Amygdala_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Amygdala_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Amygdala_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Amy_L_1_imp_p <- rbind(Amy_L_1_imp_p, demo) 
rm(col_names, demo)

Amy_L_2_imp_p <- summary(Amy_L_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Amygdala_L) 

# Amygdala - R
Amy_R_1_imp_p <- summary(Amy_R_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Amygdala_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Amygdala_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Amygdala_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Amy_R_1_imp_p <- rbind(Amy_R_1_imp_p, demo) 
rm(col_names, demo)

Amy_R_2_imp_p <- summary(Amy_R_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Amygdala_R) 

# Caudate - L
Cau_L_1_imp_p <- summary(Cau_L_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Caudate_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Caudate_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Caudate_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Cau_L_1_imp_p <- rbind(Cau_L_1_imp_p, demo) 
rm(col_names, demo)

Cau_L_2_imp_p <- summary(Cau_L_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Caudate_L) 

# Caudate - R
Cau_R_1_imp_p <- summary(Cau_R_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Caudate_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Caudate_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Caudate_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Cau_R_1_imp_p <- rbind(Cau_R_1_imp_p, demo) 
rm(col_names, demo)

Cau_R_2_imp_p <- summary(Cau_R_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Caudate_R) 

# Insula - L
Ins_L_1_imp_p <- summary(Ins_L_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'drinking_days_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Insula_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Insula_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Insula_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Ins_L_1_imp_p <- rbind(Ins_L_1_imp_p, demo) 
rm(col_names, demo)

Ins_L_2_imp_p <- summary(Ins_L_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Insula_L) 

# Insula - R
Ins_R_1_imp_p <- summary(Ins_R_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Insula_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Insula_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Insula_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Ins_R_1_imp_p <- rbind(Ins_R_1_imp_p, demo) 
rm(col_names, demo)

Ins_R_2_imp_p <- summary(Ins_R_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Insula_R) 

# Nacc - L
Nac_L_1_imp_p <- summary(Nac_L_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependence_a_rec', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Nacc_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Nacc_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Nacc_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Nac_L_1_imp_p <- rbind(Nac_L_1_imp_p, demo) 
rm(col_names, demo)

Nac_L_2_imp_p <- summary(Nac_L_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Nacc_L) 

# Nacc - R
Nac_R_1_imp_p <- summary(Nac_R_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependenceYes', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Nacc_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Nacc_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Nacc_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Nac_R_1_imp_p <- rbind(Nac_R_1_imp_p, demo) 
rm(col_names, demo)

Nac_R_2_imp_p <- summary(Nac_R_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Nacc_R) 

# Putamen - L
Put_L_1_imp_p <- summary(Put_L_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Putamen_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Putamen_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Putamen_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Put_L_1_imp_p <- rbind(Put_L_1_imp_p, demo) 
rm(col_names, demo)

Put_L_2_imp_p <- summary(Put_L_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Putamen_L) 

# Putamen - R
Put_R_1_imp_p <- summary(Put_R_1_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Putamen_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Putamen_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 2))
col_names <- c('Term', 'Putamen_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov2', 'demo_cov3')
Put_R_1_imp_p <- rbind(Put_R_1_imp_p, demo) %>% 
  mutate(Term = ifelse(Term == 'sexFemale', 'demo_cov1', Term))
row_order <- c(
  '(Intercept)', 'MedNAC', 'VisitVisit_2', 'SeqA', 'baseline_ROI_cov', 
  'cannabis_days_base', 'nicotine_days_base', 'alcohol_use_cov',
  'demo_cov1', 'demo_cov2', 'demo_cov3')
Put_R_1_imp_p <- Put_R_1_imp_p %>%
  slice(match(row_order, Term))
rm(col_names, demo, row_order)

Put_R_2_imp_p <- summary(Put_R_2_imp)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Putamen_R) 

# p-values across primary models for ROI
pvalues_primary <- ACC_1_imp_p %>% 
  full_join(Amy_L_1_imp_p, by = 'Term') %>% 
  full_join(Amy_R_1_imp_p, by = 'Term') %>% 
  full_join(Cau_L_1_imp_p, by = 'Term') %>% 
  full_join(Cau_R_1_imp_p, by = 'Term') %>% 
  full_join(Ins_L_1_imp_p, by = 'Term') %>% 
  full_join(Ins_R_1_imp_p, by = 'Term') %>% 
  full_join(Nac_L_1_imp_p, by = 'Term') %>% 
  full_join(Nac_R_1_imp_p, by = 'Term') %>% 
  full_join(Put_L_1_imp_p, by = 'Term') %>% 
  full_join(Put_R_1_imp_p, by = 'Term')

# p-vales for exploratory med*sex in primary models for ROI
pvalues_exploratory <- ACC_2_imp_p %>% 
  full_join(Amy_L_2_imp_p, by = 'Term') %>% 
  full_join(Amy_R_2_imp_p, by = 'Term') %>% 
  full_join(Cau_L_2_imp_p, by = 'Term') %>% 
  full_join(Cau_R_2_imp_p, by = 'Term') %>% 
  full_join(Ins_L_2_imp_p, by = 'Term') %>% 
  full_join(Ins_R_2_imp_p, by = 'Term') %>% 
  full_join(Nac_L_2_imp_p, by = 'Term') %>% 
  full_join(Nac_R_2_imp_p, by = 'Term') %>% 
  full_join(Put_L_2_imp_p, by = 'Term') %>% 
  full_join(Put_R_2_imp_p, by = 'Term')

rm(ACC_1_imp_p, Amy_L_1_imp_p, Amy_R_1_imp_p, Cau_L_1_imp_p, Cau_R_1_imp_p, 
   Ins_L_1_imp_p, Ins_R_1_imp_p, Nac_L_1_imp_p, Nac_R_1_imp_p,
   Put_L_1_imp_p, Put_R_1_imp_p, 
   ACC_2_imp_p, Amy_L_2_imp_p, Amy_R_2_imp_p, Cau_L_2_imp_p, Cau_R_2_imp_p,
   Ins_L_2_imp_p, Ins_R_2_imp_p, Nac_L_2_imp_p, Nac_R_2_imp_p,
   Put_L_2_imp_p, Put_R_2_imp_p)

pvalues_primary <- pvalues_primary %>% 
  t() %>% 
  janitor::row_to_names(1) %>% 
  as.data.frame()

pvalues_exploratory <- pvalues_exploratory %>% 
  t() %>% 
  janitor::row_to_names(1) %>% 
  as.data.frame()

# create FDR corrected p-values
pvalues_primary_FDR <- pvalues_primary
pvalues_exploratory_FDR <- pvalues_exploratory

pvalues_primary_FDR$MedNAC <- p.adjust(
  pvalues_primary_FDR$MedNAC, method = "fdr")
pvalues_primary_FDR$VisitVisit_2 <- p.adjust(
  pvalues_primary_FDR$VisitVisit_2, method = "fdr")
pvalues_primary_FDR$SeqA <- p.adjust(
  pvalues_primary_FDR$SeqA, method = "fdr")
pvalues_primary_FDR$baseline_ROI_cov <- p.adjust(
  pvalues_primary_FDR$baseline_ROI_cov, method = "fdr")
pvalues_primary_FDR$cannabis_days_base <- p.adjust(
  pvalues_primary_FDR$cannabis_days_base, method = "fdr")
pvalues_primary_FDR$nicotine_days_base <- p.adjust(
  pvalues_primary_FDR$nicotine_days_base, method = "fdr")
pvalues_primary_FDR$alcohol_use_cov <- p.adjust(
  pvalues_primary_FDR$alcohol_use_cov, method = "fdr")
pvalues_primary_FDR$demo_cov1 <- p.adjust(
  pvalues_primary_FDR$demo_cov1, n = 11, method = "fdr")

pvalues_exploratory_FDR$`MedNAC:sexFemale` <- p.adjust(
  pvalues_exploratory_FDR$`MedNAC:sexFemale`, method = "fdr") 

pvalues_exploratory_FDR <- pvalues_exploratory_FDR %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, round, 2)

pvalues_primary_FDR <- pvalues_primary_FDR %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate_if(is.character,as.numeric) %>% 
  mutate_if(is.numeric, round, 2)

write.csv(pvalues_primary_FDR, 
          'output/models_primary/pvalues_primary_FDR.csv')
write.csv(pvalues_exploratory_FDR, 
          'output/models_exploratory/pvalues_exploratory_FDR.csv')

rm(pvalues_primary, pvalues_primary_FDR, pvalues_exploratory, 
   pvalues_exploratory_FDR)

rm(ACC_1_imp, ACC_2_imp, 
   Amy_L_1_imp, Amy_L_2_imp, Amy_R_1_imp, Amy_R_2_imp,
   Cau_L_1_imp, Cau_L_2_imp, Cau_R_1_imp, Cau_R_2_imp, 
   Ins_L_1_imp, Ins_L_2_imp, Ins_R_1_imp, Ins_R_2_imp,
   Nac_L_1_imp, Nac_L_2_imp, Nac_R_1_imp, Nac_R_2_imp,
   Put_L_1_imp, Put_L_2_imp, Put_R_1_imp, Put_R_2_imp)


# Whole Sample: Figures -----------------------------------------------------------------

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
limits_L <- aes(ymin = 0.01, ymax = mean_Amygdala_L + sd_Amygdala_L)
limits_R <- aes(ymin = 0.01, ymax = mean_Amygdala_R + sd_Amygdala_R)
dodge <- position_dodge(width = 0.9)

# ROI: amygdala (L)
ggplot(df_fig_bar, aes(fill = sex, y = mean_Amygdala_L, x = Med)) +
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
ggplot(df_fig_bar, aes(fill = sex, y = mean_Amygdala_R, x = Med)) +
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

# Whole Sample: Outcome - Subjective Craving ---------------------------------------------

#------------------------------------------------------------------------------#
#                               AOCDS
#------------------------------------------------------------------------------#

# (1) primary model 

AOC_1_imp <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l)

# - follow-up: 
desc_med <- df_imp_2l %>% 
  group_by(Med) %>% 
  summarise(
    AOCDS_mean = mean(AOCDS),
    AOCDS_sd = sd(AOCDS))
desc_med

desc_visit <- df_imp_2l %>% 
  group_by(Visit) %>% 
  summarise(
    AOCDS_mean = mean(AOCDS),
    AOCDS_sd = sd(AOCDS))
desc_visit

rm(desc_med, desc_visit)

# - model performance check
testDispersion(AOC_1_imp) 
check_singularity(AOC_1_imp) 
check_outliers(AOC_1_imp)
AOC_1_imp_vif <- vif(AOC_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
AOC_1_imp_vif
check_model(AOC_1_imp, check = 'all') 
check_heteroscedasticity(AOC_1_imp) 

# - re-run w/o outlier
# identify outlier
outliers_list <- check_outliers(AOC_1_imp)
plot(outliers_list)
insight::get_data(AOC_1_imp)[outliers_list, ] 

# re-run models removing outlier
AOC_1_imp_out <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + drinking_days_base +
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l_out)

summary(AOC_1_imp)
summary(AOC_1_imp_out)

# means w/ and w/o outlier
AOCDS_summary <- df_imp_2l %>% 
  group_by(Med) %>% 
  summarise(
    AOCDS_mean = mean(AOCDS),
    AOCDS_sd= sd(AOCDS)) 

AOCDS_summary_out <- df_imp_2l_out %>% 
  group_by(Med) %>% 
  summarise(
    AOCDS_mean = mean(AOCDS),
    AOCDS_sd= sd(AOCDS)) 

rm(outliers_list, AOC_1_imp_out, AOCDS_summary, AOCDS_summary_out)

# - save model object
AOC_1_imp_summary <- broom::tidy(AOC_1_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

AOC_2_imp <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(AOC_2_imp) 
check_singularity(AOC_2_imp) 
check_outliers(AOC_2_imp) 
vif(AOC_2_imp) 
check_model(AOC_2_imp, check = 'all') 
check_heteroscedasticity(AOC_2_imp)

# - re-run w/o outlier
AOC_2_imp_out <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_imp_2l_out)

summary(AOC_2_imp)
summary(AOC_2_imp_out)
rm(AOC_2_imp_out, df_imp_2l_out)

# - save model object
AOC_2_imp_summary <- broom::tidy(AOC_2_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'AOCDS', NA)) %>% 
  relocate(model)

# build output file for med*sex effect
exp_int_sr <- AOC_2_imp_summary
exp_int_sr_boot <- AOC_3b_imp_summary

# (4) export relevant output
write.csv(AOC_1_imp_summary, 
          'output/models_primary/AOC_1_imp_summary.csv')
write.csv(AOC_1_imp_vif, 
          'output/models_primary/AOC_1_imp_vif.csv', row.names = TRUE)

rm(AOC_1_imp_summary, AOC_2_imp_summary, AOC_1_imp_vif)

#------------------------------------------------------------------------------#
#                               ACQ
#------------------------------------------------------------------------------#

# (1) primary model 

ACQ_1_imp <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + drinks_sum_base + 
    cannabis_days_base + nicotine_days_base + age + sex + race + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(ACQ_1_imp) 
check_singularity(ACQ_1_imp)
check_outliers(ACQ_1_imp) 
ACQ_1_imp_vif <- vif(ACQ_1_imp) %>% 
  as.data.frame() %>% 
  rename(ACC_Mid = '.') %>% 
  mutate_if(is.numeric, round, 2)
ACQ_1_imp_vif
check_model(ACQ_1_imp, check = 'all') 
check_heteroscedasticity(ACQ_1_imp)

# - follow-up with robust SE
ACQ_1_imp_robust <- robust_mixed(ACQ_1_imp, type = 'CR2')
ACQ_1_imp_robust
summary(ACQ_1_imp)

# - save model object w/robust SE
ACQ_1_imp_robust_summary <- broom::tidy(ACQ_1_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -stars) %>% 
  rename(df = statistic)

# (2) exploratory model 

ACQ_2_imp <- lmer(
  ACQ ~ Med*sex + Visit + Seq + ACQ_Baseline + drinks_sum_base +
    cannabis_days_base + nicotine_days_base + age + race + 
    (1 | record_id),
  df_imp_2l) 

# - model performance check
testDispersion(ACQ_2_imp) 
check_singularity(ACQ_2_imp) 
check_outliers(ACQ_2_imp) 
vif(ACQ_2_imp)
check_model(ACQ_2_imp, check = 'all') 
check_heteroscedasticity(ACQ_2_imp) 

# - follow-up with robust SE
ACQ_2_imp_robust <- robust_mixed(ACQ_2_imp, type = 'CR2')
ACQ_2_imp_robust
summary(ACQ_2_imp)

# - save model object w/robust SE
ACQ_2_imp_robust_summary <- broom::tidy(ACQ_2_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -stars) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  mutate(model = 'ACQ', 
         effect = 'fixed') %>% 
  relocate(model, effect, everything()) %>% 
  rename(df = statistic,
         statistic = t.stat)

# build output file for med*sex effect
exp_int_sr <- rbind(exp_int_sr, ACQ_2_imp_robust_summary) 
exp_int_sr_boot <- rbind(exp_int_sr_boot, ACQ_3b_imp_summary) 

# (4) export relevant output
write.csv(ACQ_1_imp_robust_summary, 
          'output/models_primary/ACQ_1_imp_robust_summary.csv')
write.csv(ACQ_1_imp_vif, 
          'output/models_primary/ACQ_1_imp_vif.csv',row.names = TRUE)

rm(ACQ_1_imp_robust, ACQ_2_imp_robust, 
   ACQ_1_imp_robust_summary, ACQ_2_imp_robust_summary, ACQ_1_imp_vif)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output_sr <- effectsize::eta_squared(
  AOC_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'AOCDS', variable))

acq <- effectsize::eta_squared(
  ACQ_1_imp, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACQ', variable))

eff_output_sr <- rbind(eff_output_sr, acq) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output_sr,'output/models_primary/eff_output_sr.csv')

rm(eff_output_sr, acq)

### Export med*sex results
write.csv(exp_int_sr, 'output/models_exploratory/exp_int_sr.csv')
write.csv(exp_int_sr_boot, 'output/models_exploratory/exp_int_sr_boot.csv')

rm(exp_int_sr, exp_int_sr_boot, AOC_1_imp, AOC_2_imp,
   ACQ_1_imp, ACQ_2_imp)

# Complete Cases: Outcome - ROIs  ----------------------------------------------

# exploratory models: medication x sex interaction

#------------------------------------------------------------------------------#
#                                 ACC - Mid
#------------------------------------------------------------------------------#

# (1) primary model 

ACC_1_cc <- lmer(
  ACC_Mid ~ Med + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(ACC_1_cc) 
check_singularity(ACC_1_cc) 
check_outliers(ACC_1_cc) 
vif(ACC_1_cc) 
check_model(ACC_1_cc, check = 'all') 
check_heteroscedasticity(ACC_1_cc) 

# - save model object
ACC_1_cc_summary <- broom::tidy(ACC_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

ACC_2_cc <- lmer(
  ACC_Mid ~ Med*sex + Visit + Seq + ACC_Mid_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(ACC_2_cc)
check_singularity(ACC_2_cc) 
check_outliers(ACC_2_cc)
vif(ACC_2_cc) 
check_model(ACC_2_cc, check = 'all') 
check_heteroscedasticity(ACC_2_cc)

# - save model object (interaction effect only)
ACC_2_cc_summary <- broom::tidy(ACC_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'ACC_Mid', NA)) %>% 
  relocate(model)

# (3) export relevant output
write.csv(ACC_1_cc_summary, 
          'output/models_primary/complete_cases/ACC_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- ACC_2_cc_summary 

rm(ACC_1_cc_summary, ACC_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Amygdala - L
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_L_1_cc <- lmer(
  Amygdala_L ~ Med + Visit + Seq + Amygdala_L_Baseline + 
    mini10_dependence_a_rec + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Amy_L_1_cc) 
check_singularity(Amy_L_1_cc)
check_outliers(Amy_L_1_cc) 
vif(Amy_L_1_cc) 
check_model(Amy_L_1_cc, check = 'all') 
check_heteroscedasticity(Amy_L_1_cc) 

# - save model object
Amy_L_1_cc_summary <- broom::tidy(Amy_L_1_cc) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Amy_L_2_cc <- lmer(
  Amygdala_L ~ Med*sex + Visit + Seq + Amygdala_L_Baseline + 
    mini10_dependence_a_rec + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Amy_L_2_cc) 
check_singularity(Amy_L_2_cc) 
check_outliers(Amy_L_2_cc) 
vif(Amy_L_2_cc) 
check_model(Amy_L_2_cc, check = 'all') 
check_heteroscedasticity(Amy_L_2_cc) 

# - save model object
Amy_L_2_cc_summary <- broom::tidy(Amy_L_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_L', NA)) %>% 
  relocate(model)

# (3) export relevant output
write.csv(Amy_L_1_cc_summary, 
          'output/models_primary/complete_cases/Amy_L_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_L_2_cc_summary) 

rm(Amy_L_1_cc_summary, Amy_L_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Amygdala - R
#------------------------------------------------------------------------------#

# (1) primary model 

Amy_R_1_cc <- lmer(
  Amygdala_R ~ Med + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Amy_R_1_cc) 
check_singularity(Amy_R_1_cc)
check_outliers(Amy_R_1_cc) 
vif(Amy_R_1_cc) 
check_model(Amy_R_1_cc, check = 'all') 
check_heteroscedasticity(Amy_R_1_cc) 

# - save model object
Amy_R_1_cc_summary <- broom::tidy(Amy_R_1_cc) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Amy_R_2_cc <- lmer(
  Amygdala_R ~ Med*sex + Visit + Seq + Amygdala_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Amy_R_2_cc) 
check_singularity(Amy_R_2_cc) 
check_outliers(Amy_R_2_cc) 
vif(Amy_R_2_cc) 
check_model(Amy_R_2_cc, check = 'all') 
check_heteroscedasticity(Amy_R_2_cc)

# - save model object
Amy_R_2_cc_summary <- broom::tidy(Amy_R_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Amygdala_R', NA)) %>% 
  relocate(model)

# (3) export relevant output
write.csv(Amy_R_1_cc_summary, 
          'output/models_primary/complete_cases/Amy_R_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Amy_R_2_cc_summary) 

rm(Amy_R_1_cc_summary, Amy_R_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Caudate - L
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_L_1_cc <- lmer(
  Caudate_L ~ Med + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Cau_L_1_cc) 
check_singularity(Cau_L_1_cc) 
check_outliers(Cau_L_1_cc)
vif(Cau_L_1_cc)  
check_model(Cau_L_1_cc, check = 'all') 
check_heteroscedasticity(Cau_L_1_cc)

# - save model object
Cau_L_1_cc_summary <- broom::tidy(Cau_L_1_cc) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_L_2_cc <- lmer(
  Caudate_L ~ Med*sex + Visit + Seq + Caudate_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Cau_L_2_cc) 
check_singularity(Cau_L_2_cc) 
check_outliers(Cau_L_2_cc) 
vif(Cau_L_2_cc) 
check_model(Cau_L_2_cc, check = 'all') 
check_heteroscedasticity(Cau_L_2_cc) 

# - save model object
Cau_L_2_cc_summary <- broom::tidy(Cau_L_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_L', NA)) %>% 
  relocate(model)

# (3) export relevant output
write.csv(Cau_L_1_cc_summary, 
          'output/models_primary/complete_cases/Cau_L_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_L_2_cc_summary) 

rm(Cau_L_1_cc_summary, Cau_L_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Caudate - R
#------------------------------------------------------------------------------#

# (1) primary model 

Cau_R_1_cc <- lmer(
  Caudate_R ~ Med + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Cau_R_1_cc)
check_singularity(Cau_R_1_cc) 
check_outliers(Cau_R_1_cc) 
vif(Cau_R_1_cc) 
check_model(Cau_R_1_cc, check = 'all') 
check_heteroscedasticity(Cau_R_1_cc) 

# - save model object
Cau_R_1_cc_summary <- broom::tidy(Cau_R_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Cau_R_2_cc <- lmer(
  Caudate_R ~ Med*sex + Visit + Seq + Caudate_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Cau_R_2_cc) 
check_singularity(Cau_R_2_cc)
check_outliers(Cau_R_2_cc) 
vif(Cau_R_2_cc) 
check_model(Cau_R_2_cc, check = 'all') 
check_heteroscedasticity(Cau_R_2_cc)

# - save model object
Cau_R_2_cc_summary <- broom::tidy(Cau_R_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Caudate_R', NA)) %>% 
  relocate(model)

# (3) export relevant output
write.csv(Cau_R_1_cc_summary, 
          'output/models_primary/complete_cases/Cau_R_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Cau_R_2_cc_summary) 

rm(Cau_R_1_cc_summary, Cau_R_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Insula - L
#------------------------------------------------------------------------------#

# (1) primary model 

Ins_L_1_cc <- lmer(
  Insula_L ~ Med + Visit + Seq + Insula_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Ins_L_1_cc) 
check_singularity(Ins_L_1_cc) 
check_outliers(Ins_L_1_cc) 
vif(Ins_L_1_cc) 
check_model(Ins_L_1_cc, check = 'all') 
check_heteroscedasticity(Ins_L_1_cc) 

# - save model object
Ins_L_1_cc_summary <- broom::tidy(Ins_L_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_L_2_cc <- lmer(
  Insula_L ~ Med*sex + Visit + Seq + Insula_L_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Ins_L_2_cc) 
check_singularity(Ins_L_2_cc) 
check_outliers(Ins_L_2_cc) 
vif(Ins_L_2_cc) 
check_model(Ins_L_2_cc, check = 'all') 
check_heteroscedasticity(Ins_L_2_cc) 

# - save model object
Ins_L_2_cc_summary <- broom::tidy(Ins_L_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_L', NA)) %>% 
  relocate(model)

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_L_2_cc_summary) 

# (4) export relevant output
write.csv(Ins_L_1_cc_summary, 
          'output/models_primary/complete_cases/Ins_L_1_cc_summary.csv')

rm(Ins_L_1_cc_summary, Ins_L_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Insula - R
#------------------------------------------------------------------------------#

# (1) primary model 

Ins_R_1_cc <- lmer(
  Insula_R ~ Med + Visit + Seq + Insula_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Ins_R_1_cc)
check_singularity(Ins_R_1_cc) 
check_outliers(Ins_R_1_cc) 
vif(Ins_R_1_cc) 
check_model(Ins_R_1_cc, check = 'all') 
check_heteroscedasticity(Ins_R_1_cc)

# - save model object
Ins_R_1_cc_summary <- broom::tidy(Ins_R_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Ins_R_2_cc <- lmer(
  Insula_R ~ Med*sex + Visit + Seq + Insula_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Ins_R_2_cc) 
check_singularity(Ins_R_2_cc) 
check_outliers(Ins_R_2_cc) 
vif(Ins_R_2_cc) 
check_model(Ins_R_2_cc, check = 'all') 
check_heteroscedasticity(Ins_R_2_cc)

# - save model object
Ins_R_2_cc_summary <- broom::tidy(Ins_R_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Insula_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Ins_R_1_cc_summary, 
          'output/models_primary/complete_cases/Ins_R_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Ins_R_2_cc_summary) 

rm(Ins_R_1_cc_summary, Ins_R_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Nacc - L
#------------------------------------------------------------------------------#

# (1) primary model 

Nac_L_1_cc <- lmer(
  Nacc_L ~ Med + Visit + Seq + Nacc_L_Baseline + mini10_dependence_a_rec + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Nac_L_1_cc) 
check_singularity(Nac_L_1_cc) 
check_outliers(Nac_L_1_cc) 
vif(Nac_L_1_cc) 
check_model(Nac_L_1_cc, check = 'all') 
check_heteroscedasticity(Nac_L_1_cc) 

# - save model object
Nac_L_1_cc_summary <- broom::tidy(Nac_L_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_L_2_cc <- lmer(
  Nacc_L ~ Med*sex + Visit + Seq + Nacc_L_Baseline + mini10_dependence_a_rec + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Nac_L_2_cc) 
check_singularity(Nac_L_2_cc) 
check_outliers(Nac_L_2_cc) 
vif(Nac_L_2_cc) 
check_model(Nac_L_2_cc, check = 'all') 
check_heteroscedasticity(Nac_L_2_cc) 

# - save model object
Nac_L_2_cc_summary <- broom::tidy(Nac_L_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_L_1_cc_summary, 
          'output/models_primary/complete_cases/Nac_L_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_L_2_cc_summary)  

rm(Nac_L_1_cc_summary, Nac_L_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Nacc - R
#------------------------------------------------------------------------------#

# (1) primary model

Nac_R_1_cc <- lmer(
  Nacc_R ~ Med + Visit + Seq + Nacc_R_Baseline + mini10_dependence + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Nac_R_1_cc) 
check_singularity(Nac_R_1_cc) 
check_outliers(Nac_R_1_cc) 
vif(Nac_R_1_cc) 
check_model(Nac_R_1_cc, check = 'all') 
check_heteroscedasticity(Nac_R_1_cc) 

# - save model object
Nac_R_1_cc_summary <- broom::tidy(Nac_R_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

Nac_R_2_cc <- lmer(
  Nacc_R ~ Med*sex + Visit + Seq + Nacc_R_Baseline + mini10_dependence + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Nac_R_2_cc) 
check_singularity(Nac_R_2_cc) 
check_outliers(Nac_R_2_cc)
vif(Nac_R_2_cc) 
check_model(Nac_R_2_cc, check = 'all') 
check_heteroscedasticity(Nac_R_2_cc)

# - save model object
Nac_R_2_cc_summary <- broom::tidy(Nac_R_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Nacc_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Nac_R_1_cc_summary, 
          'output/models_primary/complete_cases/Nac_R_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Nac_R_2_cc_summary) 

rm(Nac_R_1_cc_summary, Nac_R_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Putamen - L
#------------------------------------------------------------------------------#

# (1) primary model 

Put_L_1_cc <- lmer(
  Putamen_L ~ Med + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base +  
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Put_L_1_cc)
check_singularity(Put_L_1_cc) 
check_outliers(Put_L_1_cc) 
vif(Put_L_1_cc) 
check_model(Put_L_1_cc, check = 'all') 
check_heteroscedasticity(Put_L_1_cc)

# - save model object
Put_L_1_cc_summary <- broom::tidy(Put_L_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_L_2_cc <- lmer(
  Putamen_L ~ Med*sex + Visit + Seq + Putamen_L_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Put_L_2_cc) 
check_singularity(Put_L_2_cc) 
check_outliers(Put_L_2_cc) 
vif(Put_L_2_cc) 
check_model(Put_L_2_cc, check = 'all') 
check_heteroscedasticity(Put_L_2_cc) 

# - save model object
Put_L_2_cc_summary <- broom::tidy(Put_L_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_L', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_L_1_cc_summary, 
          'output/models_primary/complete_cases/Put_L_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_L_2_cc_summary) 

rm(Put_L_1_cc_summary, Put_L_2_cc_summary)

#------------------------------------------------------------------------------#
#                               Putamen - R
#------------------------------------------------------------------------------#

# (1) primary model 

Put_R_1_cc <- lmer(
  Putamen_R ~ Med + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + sex + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(Put_R_1_cc) 
check_singularity(Put_R_1_cc) 
check_outliers(Put_R_1_cc) 
vif(Put_R_1_cc) 
check_model(Put_R_1_cc, check = 'all')
check_heteroscedasticity(Put_R_1_cc) 

# - save model object
Put_R_1_cc_summary <- broom::tidy(Put_R_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2) 

# (2) exploratory model 

Put_R_2_cc <- lmer(
  Putamen_R ~ Med*sex + Visit + Seq + Putamen_R_Baseline + dpdd_base + 
    cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(Put_R_2_cc) 
check_singularity(Put_R_2_cc)
check_outliers(Put_R_2_cc)
vif(Put_R_2_cc) 
check_model(Put_R_2_cc, check = 'all') 
check_heteroscedasticity(Put_R_2_cc)

# - save model object
Put_R_2_cc_summary <- broom::tidy(Put_R_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'Putamen_R', NA)) %>% 
  relocate(model)

# (4) export relevant output
write.csv(Put_R_1_cc_summary, 
          'output/models_primary/complete_cases/Put_R_1_cc_summary.csv')

# build output file for med*sex effect
exp_int <- rbind(exp_int, Put_R_2_cc_summary) 

rm(Put_R_1_cc_summary, Put_R_2_cc_summary)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output <- effectsize::eta_squared(
  ACC_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% # retain med effect size for Med effect only
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACC', variable))

amy_L <- effectsize::eta_squared(
  Amy_L_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (L)', variable))

amy_R <- effectsize::eta_squared(
  Amy_R_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Amygdala (R)', variable))

cau_L <- effectsize::eta_squared(
  Cau_L_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (L)', variable))

cau_R <- effectsize::eta_squared(
  Cau_R_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Caudate (R)', variable))

ins_L <- effectsize::eta_squared(
  Ins_L_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (L)', variable))

ins_R <- effectsize::eta_squared(
  Ins_R_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Insula (R)', variable))

nac_L <- effectsize::eta_squared(
  Nac_L_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (L)', variable))

nac_R <- effectsize::eta_squared(
  Nac_R_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(
    variable == 'Med', 'Nucleus Accumbens (R)', variable))

put_L <- effectsize::eta_squared(
  Put_L_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (L)', variable))

put_R <- effectsize::eta_squared(
  Put_R_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'Putamen (R)', variable))

eff_output <- rbind(eff_output, amy_L, amy_R, cau_L, cau_R, 
                    ins_L, ins_R, nac_L, nac_R, put_L, put_R) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output,'output/models_primary/complete_cases/eff_output.csv')

rm(amy_L, amy_R, cau_L, cau_R, ins_L, ins_R, nac_L, nac_R, 
   put_L, put_R, eff_output)

### Export med*sex results
write.csv(exp_int, 'output/models_exploratory/complete_cases/exp_int.csv')

rm(exp_int)

# Complete Cases: ROI Multiple Comparison Correction -----------------------------------------------------------------------

# ACC - Mid
ACC_1_cc_p <- summary(ACC_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(ACC_Mid = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'ACC_Mid_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, ACC_Mid) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'ACC_Mid')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
ACC_1_cc_p <- rbind(ACC_1_cc_p, demo) 
rm(col_names, demo) 

ACC_2_cc_p <- summary(ACC_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(ACC_Mid = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, ACC_Mid)  

# Amygdala - L
Amy_L_1_cc_p <- summary(Amy_L_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependence_a_rec', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Amygdala_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Amygdala_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Amygdala_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Amy_L_1_cc_p <- rbind(Amy_L_1_cc_p, demo) 
rm(col_names, demo)

Amy_L_2_cc_p <- summary(Amy_L_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Amygdala_L) 

# Amygdala - R
Amy_R_1_cc_p <- summary(Amy_R_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Amygdala_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Amygdala_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Amygdala_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Amy_R_1_cc_p <- rbind(Amy_R_1_cc_p, demo) 
rm(col_names, demo)

Amy_R_2_cc_p <- summary(Amy_R_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Amygdala_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Amygdala_R) 

# Caudate - L
Cau_L_1_cc_p <- summary(Cau_L_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Caudate_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Caudate_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Caudate_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Cau_L_1_cc_p <- rbind(Cau_L_1_cc_p, demo) 
rm(col_names, demo)

Cau_L_2_cc_p <- summary(Cau_L_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Caudate_L) 

# Caudate - R
Cau_R_1_cc_p <- summary(Cau_R_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Caudate_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Caudate_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Caudate_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Cau_R_1_cc_p <- rbind(Cau_R_1_cc_p, demo) 
rm(col_names, demo)

Cau_R_2_cc_p <- summary(Cau_R_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Caudate_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Caudate_R) 

# Insula - L
Ins_L_1_cc_p <- summary(Ins_L_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'drinking_days_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Insula_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Insula_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Insula_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Ins_L_1_cc_p <- rbind(Ins_L_1_cc_p, demo) 
rm(col_names, demo)

Ins_L_2_cc_p <- summary(Ins_L_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Insula_L) 

# Insula - R
Ins_R_1_cc_p <- summary(Ins_R_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Insula_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Insula_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Insula_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Ins_R_1_cc_p <- rbind(Ins_R_1_cc_p, demo) 
rm(col_names, demo)

Ins_R_2_cc_p <- summary(Ins_R_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Insula_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Insula_R) 

# Nacc - L
Nac_L_1_cc_p <- summary(Nac_L_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependence_a_rec', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Nacc_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Nacc_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Nacc_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Nac_L_1_cc_p <- rbind(Nac_L_1_cc_p, demo) 
rm(col_names, demo)

Nac_L_2_cc_p <- summary(Nac_L_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Nacc_L) 

# Nacc - R
Nac_R_1_cc_p <- summary(Nac_R_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'mini10_dependenceYes', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Nacc_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Nacc_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Nacc_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Nac_R_1_cc_p <- rbind(Nac_R_1_cc_p, demo) 
rm(col_names, demo)

Nac_R_2_cc_p <- summary(Nac_R_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Nacc_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Nacc_R) 

# Putamen - L
Put_L_1_cc_p <- summary(Put_L_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Putamen_L_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Putamen_L) 
demo <- data.frame(matrix(ncol = 2, nrow = 3))
col_names <- c('Term', 'Putamen_L')
colnames(demo) <- col_names
demo$Term <- c('demo_cov1', 'demo_cov2', 'demo_cov3')
Put_L_1_cc_p <- rbind(Put_L_1_cc_p, demo) 
rm(col_names, demo)

Put_L_2_cc_p <- summary(Put_L_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_L = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Putamen_L) 

# Putamen - R
Put_R_1_cc_p <- summary(Put_R_1_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  mutate(
    Term = ifelse(Term == 'dpdd_base', 'alcohol_use_cov', Term),
    Term = ifelse(Term == 'Putamen_R_Baseline', 'baseline_ROI_cov', Term)) %>% 
  select(Term, Putamen_R) 
demo <- data.frame(matrix(ncol = 2, nrow = 2))
col_names <- c('Term', 'Putamen_R')
colnames(demo) <- col_names
demo$Term <- c('demo_cov2', 'demo_cov3')
Put_R_1_cc_p <- rbind(Put_R_1_cc_p, demo) %>% 
  mutate(Term = ifelse(Term == 'sexFemale', 'demo_cov1', Term))
row_order <- c(
  '(Intercept)', 'MedNAC', 'VisitVisit_2', 'SeqA', 'baseline_ROI_cov', 
  'cannabis_days_base', 'nicotine_days_base', 'alcohol_use_cov',
  'demo_cov1', 'demo_cov2', 'demo_cov3')
Put_R_1_cc_p <- Put_R_1_cc_p %>%
  slice(match(row_order, Term))
rm(col_names, demo, row_order)

Put_R_2_cc_p <- summary(Put_R_2_cc)[["coefficients"]] %>% 
  as.data.frame() %>%  
  rename(Putamen_R = 'Pr(>|t|)') %>% 
  rownames_to_column(., 'Term') %>% 
  tail(1) %>% 
  select(Term, Putamen_R) 

# p-values across primary models for ROI
pvalues_primary <- ACC_1_cc_p %>% 
  full_join(Amy_L_1_cc_p, by = 'Term') %>% 
  full_join(Amy_R_1_cc_p, by = 'Term') %>% 
  full_join(Cau_L_1_cc_p, by = 'Term') %>% 
  full_join(Cau_R_1_cc_p, by = 'Term') %>% 
  full_join(Ins_L_1_cc_p, by = 'Term') %>% 
  full_join(Ins_R_1_cc_p, by = 'Term') %>% 
  full_join(Nac_L_1_cc_p, by = 'Term') %>% 
  full_join(Nac_R_1_cc_p, by = 'Term') %>% 
  full_join(Put_L_1_cc_p, by = 'Term') %>% 
  full_join(Put_R_1_cc_p, by = 'Term')

# p-vales for exploratory med*sex in primary models for ROI
pvalues_exploratory <- ACC_2_cc_p %>% 
  full_join(Amy_L_2_cc_p, by = 'Term') %>% 
  full_join(Amy_R_2_cc_p, by = 'Term') %>% 
  full_join(Cau_L_2_cc_p, by = 'Term') %>% 
  full_join(Cau_L_2_cc_p, by = 'Term') %>% 
  full_join(Ins_L_2_cc_p, by = 'Term') %>% 
  full_join(Ins_R_2_cc_p, by = 'Term') %>% 
  full_join(Nac_L_2_cc_p, by = 'Term') %>% 
  full_join(Nac_R_2_cc_p, by = 'Term') %>% 
  full_join(Put_L_2_cc_p, by = 'Term') %>% 
  full_join(Put_R_2_cc_p, by = 'Term')

rm(ACC_1_cc_p, Amy_L_1_cc_p, Amy_R_1_cc_p, Cau_L_1_cc_p, Cau_R_1_cc_p, 
   Ins_L_1_cc_p, Ins_R_1_cc_p, Nac_L_1_cc_p, Nac_R_1_cc_p,
   Put_L_1_cc_p, Put_R_1_cc_p, 
   ACC_2_cc_p, Amy_L_2_cc_p, Amy_R_2_cc_p, Cau_L_2_cc_p, Cau_R_2_cc_p,
   Ins_L_2_cc_p, Ins_R_2_cc_p, Nac_L_2_cc_p, Nac_R_2_cc_p,
   Put_L_2_cc_p, Put_R_2_cc_p)

pvalues_primary <- pvalues_primary %>% 
  t() %>% 
  janitor::row_to_names(1) %>% 
  as.data.frame()

pvalues_exploratory <- pvalues_exploratory %>% 
  t() %>% 
  janitor::row_to_names(1) %>% 
  as.data.frame()

# create FDR corrected p-values
pvalues_primary_FDR <- pvalues_primary
pvalues_exploratory_FDR <- pvalues_exploratory

pvalues_primary_FDR$MedNAC <- p.adjust(
  pvalues_primary_FDR$MedNAC, method = "fdr")
pvalues_primary_FDR$VisitVisit_2 <- p.adjust(
  pvalues_primary_FDR$VisitVisit_2, method = "fdr")
pvalues_primary_FDR$SeqA <- p.adjust(
  pvalues_primary_FDR$SeqA, method = "fdr")
pvalues_primary_FDR$baseline_ROI_cov <- p.adjust(
  pvalues_primary_FDR$baseline_ROI_cov, method = "fdr")
pvalues_primary_FDR$cannabis_days_base <- p.adjust(
  pvalues_primary_FDR$cannabis_days_base, method = "fdr")
pvalues_primary_FDR$nicotine_days_base <- p.adjust(
  pvalues_primary_FDR$nicotine_days_base, method = "fdr")
pvalues_primary_FDR$alcohol_use_cov <- p.adjust(
  pvalues_primary_FDR$alcohol_use_cov, method = "fdr")
pvalues_primary_FDR$demo_cov1 <- p.adjust(
  pvalues_primary_FDR$demo_cov1, n = 11, method = "fdr")

pvalues_exploratory_FDR$`MedNAC:sexFemale` <- p.adjust(
  pvalues_exploratory_FDR$`MedNAC:sexFemale`, method = "fdr") 

pvalues_exploratory_FDR <- pvalues_exploratory_FDR %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, round, 2)

pvalues_primary_FDR <- pvalues_primary_FDR %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate_if(is.character,as.numeric) %>% 
  mutate_if(is.numeric, round, 2)

write.csv(pvalues_primary_FDR, 
          'output/models_primary/complete_cases/pvalues_primary_FDR.csv')
write.csv(pvalues_exploratory_FDR, 
          'output/models_exploratory/complete_cases/pvalues_exploratory_FDR.csv')

rm(pvalues_primary, pvalues_primary_FDR, pvalues_exploratory, 
   pvalues_exploratory_FDR)

rm(ACC_1_cc, ACC_2_cc, 
   Amy_L_1_cc, Amy_L_2_cc, Amy_R_1_cc, Amy_R_2_cc,
   Cau_L_1_cc, Cau_L_2_cc, Cau_R_1_cc, Cau_R_2_cc, 
   Ins_L_1_cc, Ins_L_2_cc, Ins_R_1_cc, Ins_R_2_cc,
   Nac_L_1_cc, Nac_L_2_cc, Nac_R_1_cc, Nac_R_2_cc,
   Put_L_1_cc, Put_L_2_cc, Put_R_1_cc, Put_R_2_cc)

# Complete Cases: Outcome - Subjective Craving ---------------------------------

#------------------------------------------------------------------------------#
#                               AOCDS
#------------------------------------------------------------------------------#

# (1) primary model 

AOC_1_cc <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(AOC_1_cc) 
check_singularity(AOC_1_cc) 
check_outliers(AOC_1_cc) 
vif(AOC_1_cc) 
check_model(AOC_1_cc, check = 'all') 
check_heteroscedasticity(AOC_1_cc)

# - re-run w/o outlier

# identify outlier
outliers_list <- check_outliers(AOC_1_cc)
plot(outliers_list)
insight::get_data(AOC_1_cc)[outliers_list, ] 

AOC_1_cc_out <- lmer(
  AOCDS ~ Med + Visit + Seq + AOCDS_Baseline + drinking_days_base +
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_cc_2l_out)

summary(AOC_1_cc)
summary(AOC_1_cc_out)

rm(outliers_list, AOC_1_cc_out, AOCDS_summary, AOCDS_summary_out)

# - save model object
AOC_1_cc_summary <- broom::tidy(AOC_1_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group) %>% 
  filter(row_number() <= n()-2)

# (2) exploratory model 

AOC_2_cc <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(AOC_2_cc)
check_singularity(AOC_2_cc) 
check_outliers(AOC_2_cc) 
vif(AOC_2_cc) 
check_model(AOC_2_cc, check = 'all') 
check_heteroscedasticity(AOC_2_cc) 

# - re-run w/o outlier
AOC_2_cc_out <- lmer(
  AOCDS ~ Med*sex + Visit + Seq + AOCDS_Baseline + drinking_days_base + 
    cannabis_days_base + nicotine_days_base + race + 
    (1 | record_id),
  df_cc_2l_out)

summary(AOC_2_cc)
summary(AOC_2_cc_out)
rm(AOC_2_cc_out, df_cc_2l_out)

# - save model object
AOC_2_cc_summary <- broom::tidy(AOC_2_cc) %>%
  mutate_if(is.numeric, round, 2) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  rename(model = group) %>% 
  mutate(model = ifelse(is.na(model), 'AOCDS', NA)) %>% 
  relocate(model)

# build output file for med*sex effect
exp_int_sr <- AOC_2_cc_summary

# (4) export relevant output
write.csv(AOC_1_cc_summary, 
          'output/models_primary/complete_cases/AOC_1_cc_summary.csv')

rm(AOC_1_cc_summary, AOC_2_cc_summary)

#------------------------------------------------------------------------------#
#                               ACQ
#------------------------------------------------------------------------------#

# (1) primary model 

ACQ_1_cc <- lmer(
  ACQ ~ Med + Visit + Seq + ACQ_Baseline + drinks_sum_base + 
    cannabis_days_base + nicotine_days_base + age + sex + race + 
    (1 | record_id),
  df_cc_2l)

# - model performance check
testDispersion(ACQ_1_cc) 
check_singularity(ACQ_1_cc) 
check_outliers(ACQ_1_cc) 
vif(ACQ_1_cc)
check_model(ACQ_1_cc, check = 'all') 
check_heteroscedasticity(ACQ_1_cc)

# - follow-up with robust SE
ACQ_1_cc_robust <- robust_mixed(ACQ_1_cc, type = 'CR2')
ACQ_1_cc_robust
summary(ACQ_1_cc)

# - save model object w/robust SE
ACQ_1_cc_robust_summary <- broom::tidy(ACQ_1_cc_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -stars) %>% 
  rename(df = statistic)

# (2) exploratory model 

ACQ_2_cc <- lmer(
  ACQ ~ Med*sex + Visit + Seq + ACQ_Baseline + drinks_sum_base +
    cannabis_days_base + nicotine_days_base + age + race + 
    (1 | record_id),
  df_cc_2l) 

# - model performance check
testDispersion(ACQ_2_cc) 
check_singularity(ACQ_2_cc) 
check_outliers(ACQ_2_cc) 
vif(ACQ_2_cc)
check_model(ACQ_2_cc, check = 'all') 
check_heteroscedasticity(ACQ_2_cc)

# - follow-up with robust SE
ACQ_2_cc_robust <- robust_mixed(ACQ_2_cc, type = 'CR2')
ACQ_2_cc_robust
summary(ACQ_2_cc)

# - save model object w/robust SE
ACQ_2_cc_robust_summary <- broom::tidy(ACQ_2_cc_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -stars) %>% 
  subset(term == 'MedNAC:sexFemale') %>% 
  mutate(model = 'ACQ', 
         effect = 'fixed') %>% 
  relocate(model, effect, everything()) %>% 
  rename(df = statistic,
         statistic = t.stat)

# build output file for med*sex effect
exp_int_sr <- rbind(exp_int_sr, ACQ_2_cc_robust_summary) 

# (3) export relevant output
write.csv(ACQ_1_cc_robust_summary, 
          'output/models_primary/complete_cases/ACQ_1_cc_robust_summary.csv')

rm(ACQ_1_cc_robust, ACQ_2_cc_robust, 
   ACQ_1_cc_robust_summary, ACQ_2_cc_robust_summary)

#------------------------------------------------------------------------------#
#                        Export effect size for med effect
#------------------------------------------------------------------------------#

eff_output_sr <- effectsize::eta_squared(
  AOC_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'AOCDS', variable))

acq <- effectsize::eta_squared(
  ACQ_1_cc, partial = TRUE, alternative = 'two.sided') %>% 
  as.data.frame() %>% 
  head(1) %>% 
  rename(variable = 'Parameter') %>% 
  mutate(variable = ifelse(variable == 'Med', 'ACQ', variable))

eff_output_sr <- rbind(eff_output_sr, acq) %>% 
  mutate_if(is.numeric, round, 2) 

write.csv(eff_output_sr,'output/models_primary/complete_cases/eff_output_sr.csv')

rm(eff_output_sr, acq)

### Export med*sex results
write.csv(exp_int_sr, 'output/models_exploratory/exp_int_sr.csv')

rm(exp_int_sr, AOC_1_cc, AOC_2_cc, ACQ_1_cc, ACQ_2_cc)

# Exploratory TLFB: Drinks per Drinking Day -------------------------------

#------------------------------------------------------------------------------#
#               primary predictor: baseline ACC_Mid*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_ACC_imp <- lmer(
  dpdd ~ ACC_Mid_Baseline*Med + 
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_ACC_imp) 
check_singularity(dpdd_ACC_imp) 
check_outliers(dpdd_ACC_imp) 
vif(dpdd_ACC_imp) 
check_model(dpdd_ACC_imp, check = 'all') 
check_heteroscedasticity(dpdd_ACC_imp) 

# - save model object
dpdd_summary <- broom::tidy(dpdd_ACC_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 
names(dpdd_summary)

row_order <- c(
  'Model', 'ACC_Mid_Baseline:MedNAC', 'ACC_Mid_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

dpdd_summary <- dpdd_summary %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline:MedNAC' ~ 'Interaction: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline' ~ 'ROI: ACC_Mid (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

rm(dpdd_ACC_imp, row_order)

#------------------------------------------------------------------------------#
#              primary predictor: baseline Amygdala_L*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Amy_L_imp <- lmer(
  dpdd ~ Amygdala_L_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Amy_L_imp) 
check_singularity(dpdd_Amy_L_imp)
check_outliers(dpdd_Amy_L_imp) 
vif(dpdd_Amy_L_imp) 
check_model(dpdd_Amy_L_imp, check = 'all') 
check_heteroscedasticity(dpdd_Amy_L_imp) 

# - save model object
temp <- broom::tidy(dpdd_Amy_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_L_Baseline:MedNAC', 'Amygdala_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline:MedNAC' ~ 'Interaction: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline' ~ 'ROI: Amygdala (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Amy_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Amygdala_R*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Amy_R_imp <- lmer(
  dpdd ~ Amygdala_R_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Amy_R_imp) 
check_singularity(dpdd_Amy_R_imp)
check_outliers(dpdd_Amy_R_imp) 
vif(dpdd_Amy_R_imp) 
check_model(dpdd_Amy_R_imp, check = 'all') 
check_heteroscedasticity(dpdd_Amy_R_imp) 

# - save model object
temp <- broom::tidy(dpdd_Amy_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_R_Baseline:MedNAC', 'Amygdala_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline:MedNAC' ~ 'Interaction: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline' ~ 'ROI: Amygdala (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Amy_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Caudate_L*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Cau_L_imp <- lmer(
  dpdd ~ Caudate_L_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Cau_L_imp) 
check_singularity(dpdd_Cau_L_imp)
check_outliers(dpdd_Cau_L_imp) 
vif(dpdd_Cau_L_imp) 
check_model(dpdd_Cau_L_imp, check = 'all')
check_heteroscedasticity(dpdd_Cau_L_imp) 

# - save model object
temp <- broom::tidy(dpdd_Cau_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_L_Baseline:MedNAC', 'Caudate_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_L x Med',
    term == 'Caudate_L_Baseline:MedNAC' ~ 'Interaction: Caudate_L x Med',
    term == 'Caudate_L_Baseline' ~ 'ROI: Caudate (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Cau_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                  primary predictor: baseline Caudate_R*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Cau_R_imp <- lmer(
  dpdd ~ Caudate_R_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Cau_R_imp) 
check_singularity(dpdd_Cau_R_imp)
check_outliers(dpdd_Cau_R_imp) 
vif(dpdd_Cau_R_imp) 
check_model(dpdd_Cau_R_imp, check = 'all') 
check_heteroscedasticity(dpdd_Cau_R_imp) 

# - save model object
temp <- broom::tidy(dpdd_Cau_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_R_Baseline:MedNAC', 'Caudate_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_R x Med',
    term == 'Caudate_R_Baseline:MedNAC' ~ 'Interaction: Caudate_R x Med',
    term == 'Caudate_R_Baseline' ~ 'ROI: Caudate (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Cau_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Insula_L*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Ins_L_imp <- lmer(
  dpdd ~ Insula_L_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Ins_L_imp)
check_singularity(dpdd_Ins_L_imp) 
check_outliers(dpdd_Ins_L_imp) 
vif(dpdd_Ins_L_imp) 
check_model(dpdd_Ins_L_imp, check = 'all') 
check_heteroscedasticity(dpdd_Ins_L_imp) 

# - save model object
temp <- broom::tidy(dpdd_Ins_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_L_Baseline:MedNAC', 'Insula_L_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_L x Med',
    term == 'Insula_L_Baseline:MedNAC' ~ 'Interaction: Insula_L x Med',
    term == 'Insula_L_Baseline' ~ 'ROI: Insula (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Ins_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Insula_R*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Ins_R_imp <- lmer(
  dpdd ~ Insula_R_Baseline*Med + 
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Ins_R_imp) 
check_singularity(dpdd_Ins_R_imp)
check_outliers(dpdd_Ins_R_imp) 
vif(dpdd_Ins_R_imp) 
check_model(dpdd_Ins_R_imp, check = 'all') 
check_heteroscedasticity(dpdd_Ins_R_imp) 

# - save model object
temp <- broom::tidy(dpdd_Ins_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_R_Baseline:MedNAC', 'Insula_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_R x Med',
    term == 'Insula_R_Baseline:MedNAC' ~ 'Interaction: Insula_R x Med',
    term == 'Insula_R_Baseline' ~ 'ROI: Insula (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%  
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

# - follow-up on interaction
temp_2 <- df_imp_2l %>% 
  group_by(Med) %>% 
  summarise(
    mean_dpdd = mean(dpdd),
    sd_dpdd = sd(dpdd))
temp_2

# figure

df_fig <- df_imp_2l %>% 
  mutate(
    Medication = case_when(
      Med == 'NAC' ~ 'N-acetylcysteine',
      Med == 'Placebo' ~ 'Placebo'))

ggplot(data = df_fig, 
       aes(fill = Medication, x = Insula_R_Baseline, y = dpdd, color = Medication)) +
  geom_smooth(method='lm', se = FALSE, formula= y~x) +
  theme_classic(base_size = 18) +
  labs(x ="ROI: Insula (R)", y = "Drinks per Drinking Day", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial")) 

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Ins_R_imp, temp, temp_2, row_order, df_fig)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Nacc_L*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Nac_L_imp <- lmer(
  dpdd ~ Nacc_L_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Nac_L_imp) 
check_singularity(dpdd_Nac_L_imp) 
check_outliers(dpdd_Nac_L_imp) 
vif(dpdd_Nac_L_imp) 
check_model(dpdd_Nac_L_imp, check = 'all') 
check_heteroscedasticity(dpdd_Nac_L_imp) 

# - save model object
temp <- broom::tidy(dpdd_Nac_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_L_Baseline:MedNAC', 'Nacc_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_L x Med',
    term == 'Nacc_L_Baseline:MedNAC' ~ 'Interaction: Nacc_L x Med',
    term == 'Nacc_L_Baseline' ~ 'ROI: Nacc (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%   
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Nac_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                  primary predictor: baseline Nacc_R*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Nac_R_imp <- lmer(
  dpdd ~ Nacc_R_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Nac_R_imp) 
check_singularity(dpdd_Nac_R_imp) 
check_outliers(dpdd_Nac_R_imp) 
vif(dpdd_Nac_R_imp) 
check_model(dpdd_Nac_R_imp, check = 'all') 
check_heteroscedasticity(dpdd_Nac_R_imp) 

# - save model object
temp <- broom::tidy(dpdd_Nac_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_R_Baseline:MedNAC', 'Nacc_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_R x Med',
    term == 'Nacc_R_Baseline:MedNAC' ~ 'Interaction: Nacc_R x Med',
    term == 'Nacc_R_Baseline' ~ 'ROI: Nacc (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%   
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Nac_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                  primary predictor: baseline Putamen_L*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Put_L_imp <- lmer(
  dpdd ~ Putamen_L_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Put_L_imp) 
check_singularity(dpdd_Put_L_imp) 
check_outliers(dpdd_Put_L_imp)
vif(dpdd_Put_L_imp) 
check_model(dpdd_Put_L_imp, check = 'all') 
check_heteroscedasticity(dpdd_Put_L_imp) 

# - save model object
temp <- broom::tidy(dpdd_Put_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_L_Baseline:MedNAC', 'Putamen_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_L x Med',
    term == 'Putamen_L_Baseline:MedNAC' ~ 'Interaction: Putamen_L x Med',
    term == 'Putamen_L_Baseline' ~ 'ROI: Putamen (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%   
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Put_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Putamen_R*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_Put_R_imp <- lmer(
  dpdd ~ Putamen_R_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_Put_R_imp) 
check_singularity(dpdd_Put_R_imp) 
check_outliers(dpdd_Put_R_imp) 
vif(dpdd_Put_R_imp)
check_model(dpdd_Put_R_imp, check = 'all') 
check_heteroscedasticity(dpdd_Put_R_imp)

# - save model object
temp <- broom::tidy(dpdd_Put_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_R_Baseline:MedNAC', 'Putamen_R_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_R x Med',
    term == 'Putamen_R_Baseline:MedNAC' ~ 'Interaction: Putamen_R x Med',
    term == 'Putamen_R_Baseline' ~ 'ROI: Putamen (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_Put_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline AOCDS*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_AOCDS_imp <- lmer(
  dpdd ~ AOCDS_Baseline*Med +
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_AOCDS_imp) 
check_singularity(dpdd_AOCDS_imp) 
check_outliers(dpdd_AOCDS_imp)
vif(dpdd_AOCDS_imp) 
check_model(dpdd_AOCDS_imp, check = 'all') 
check_heteroscedasticity(dpdd_AOCDS_imp) 

# - save model object
temp <- broom::tidy(dpdd_AOCDS_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'AOCDS_Baseline:MedNAC', 'AOCDS_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'dpdd_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: AOCDS x Med',
    term == 'AOCDS_Baseline:MedNAC' ~ 'Interaction: AOCDS x Med',
    term == 'AOCDS_Baseline' ~ 'AOCDS (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_AOCDS_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline ACQ*Med
#------------------------------------------------------------------------------#

# - initial model
dpdd_ACQ_imp <- lmer(
  dpdd ~ ACQ_Baseline*Med + 
    # covariates
    Visit + Seq + dpdd_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(dpdd_ACQ_imp) 
check_singularity(dpdd_ACQ_imp) 
check_outliers(dpdd_ACQ_imp) 
vif(dpdd_ACQ_imp) 
check_model(dpdd_ACQ_imp, check = 'all') 
check_heteroscedasticity(dpdd_ACQ_imp) 

# - save model object
temp <- broom::tidy(dpdd_ACQ_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'ACQ_Baseline:MedNAC', 'ACQ_Baseline', 'MedNAC', 'VisitVisit_2', 
  'SeqA', 'dpdd_base', 'cannabis_days_base', 'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACQ x Med',
    term == 'ACQ_Baseline:MedNAC' ~ 'Interaction: ACQ x Med',
    term == 'ACQ_Baseline' ~ 'ACQ (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'dpdd_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>%
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

dpdd_summary <- rbind(dpdd_summary, temp)
rm(dpdd_ACQ_imp, temp, row_order)

### export final results

# - update if value is 0, change to <0.01

dpdd_summary <- dpdd_summary %>% 
  mutate(
    Estimate = ifelse(Estimate == 0.00, '<0.01', Estimate),
    SE = ifelse(SE == 0.00, '<0.01', SE),
    `t-statistic` = ifelse(`t-statistic` == 0.00, '<0.01', `t-statistic`),
    p = ifelse(p == 0.00, '<0.01', p))

write.csv(dpdd_summary, 'output/models_exploratory/TLFB/dpdd_summary.csv')
rm(dpdd_summary)
# Exploratory TLFB: Total Drinks -----------------------------------------------

#------------------------------------------------------------------------------#
#               primary predictor: baseline ACC_Mid*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_ACC_imp <- lmer(
  drinks_sum ~ ACC_Mid_Baseline*Med +  
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_ACC_imp) 
check_singularity(drinks_sum_ACC_imp)
check_outliers(drinks_sum_ACC_imp) 
vif(drinks_sum_ACC_imp) 
check_model(drinks_sum_ACC_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_ACC_imp) 

# - follow-up with robust SE
drinks_sum_ACC_imp_robust <- robust_mixed(drinks_sum_ACC_imp, type = 'CR2')
drinks_sum_ACC_imp_robust

# - save model object (w/robust SE)
drinks_sum_summary <- broom::tidy(drinks_sum_ACC_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 
names(drinks_sum_summary)

row_order <- c(
  'Model', 'ACC_Mid_Baseline:MedNAC', 'ACC_Mid_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

drinks_sum_summary <- drinks_sum_summary %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline:MedNAC' ~ 'Interaction: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline' ~ 'ROI: ACC_Mid (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

rm(drinks_sum_ACC_imp, drinks_sum_ACC_imp_robust, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Amygdala_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Amy_L_imp <- lmer(
  drinks_sum ~ Amygdala_L_Baseline*Med +
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Amy_L_imp) 
check_singularity(drinks_sum_Amy_L_imp) 
check_outliers(drinks_sum_Amy_L_imp) 
vif(drinks_sum_Amy_L_imp)
check_model(drinks_sum_Amy_L_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Amy_L_imp) 

# - follow-up with robust SE
drinks_sum_Amy_L_imp_robust <- robust_mixed(drinks_sum_Amy_L_imp, type = 'CR2')
drinks_sum_Amy_L_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Amy_L_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_L_Baseline:MedNAC', 'Amygdala_L_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline:MedNAC' ~ 'Interaction: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline' ~ 'ROI: Amygdala (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Amy_L_imp, drinks_sum_Amy_L_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Amygdala_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Amy_R_imp <- lmer(
  drinks_sum ~ Amygdala_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Amy_R_imp) 
check_singularity(drinks_sum_Amy_R_imp) 
check_outliers(drinks_sum_Amy_R_imp)
vif(drinks_sum_Amy_R_imp) 
check_model(drinks_sum_Amy_R_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Amy_R_imp) 

# - follow-up with robust SE
drinks_sum_Amy_R_imp_robust <- robust_mixed(drinks_sum_Amy_R_imp, type = 'CR2')
drinks_sum_Amy_R_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Amy_R_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_R_Baseline:MedNAC', 'Amygdala_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline:MedNAC' ~ 'Interaction: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline' ~ 'ROI: Amygdala (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Amy_R_imp, drinks_sum_Amy_R_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Caudate_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Cau_L_imp <- lmer(
  drinks_sum ~ Caudate_L_Baseline*Med +
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Cau_L_imp) 
check_singularity(drinks_sum_Cau_L_imp) 
check_outliers(drinks_sum_Cau_L_imp) 
vif(drinks_sum_Cau_L_imp) 
check_model(drinks_sum_Cau_L_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Cau_L_imp) 

# - follow-up with robust SE
drinks_sum_Cau_L_imp_robust <- robust_mixed(drinks_sum_Cau_L_imp, type = 'CR2')
drinks_sum_Cau_L_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Cau_L_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_L_Baseline:MedNAC', 'Caudate_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_L x Med',
    term == 'Caudate_L_Baseline:MedNAC' ~ 'Interaction: Caudate_L x Med',
    term == 'Caudate_L_Baseline' ~ 'ROI: Caudate (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Cau_L_imp, drinks_sum_Cau_L_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Caudate_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Cau_R_imp <- lmer(
  drinks_sum ~ Caudate_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Cau_R_imp)
check_singularity(drinks_sum_Cau_R_imp) 
check_outliers(drinks_sum_Cau_R_imp)
vif(drinks_sum_Cau_R_imp) 
check_model(drinks_sum_Cau_R_imp, check = 'all')
check_heteroscedasticity(drinks_sum_Cau_R_imp) 

# - follow-up: drop age
temp <- lmer(
  drinks_sum ~ Caudate_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + race +
    (1 | record_id),
  df_imp_2l)
summary(drinks_sum_Cau_R_imp)
summary(temp)

# - follow-up with robust SE
drinks_sum_Cau_R_imp_robust <- robust_mixed(drinks_sum_Cau_R_imp, type = 'CR2')
drinks_sum_Cau_R_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Cau_R_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_R_Baseline:MedNAC', 'Caudate_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_R x Med',
    term == 'Caudate_R_Baseline:MedNAC' ~ 'Interaction: Caudate_R x Med',
    term == 'Caudate_R_Baseline' ~ 'ROI: Caudate (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Cau_R_imp, drinks_sum_Cau_R_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Insula_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Ins_L_imp <- lmer(
  drinks_sum ~ Insula_L_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Ins_L_imp) 
check_singularity(drinks_sum_Ins_L_imp) 
check_outliers(drinks_sum_Ins_L_imp) 
vif(drinks_sum_Ins_L_imp) 
check_model(drinks_sum_Ins_L_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Ins_L_imp) 

# - follow-up with robust SE
drinks_sum_Ins_L_imp_robust <- robust_mixed(drinks_sum_Ins_L_imp, type = 'CR2')
drinks_sum_Ins_L_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Ins_L_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_L_Baseline:MedNAC', 'Insula_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_L x Med',
    term == 'Insula_L_Baseline:MedNAC' ~ 'Interaction: Insula_L x Med',
    term == 'Insula_L_Baseline' ~ 'ROI: Insula (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Ins_L_imp, drinks_sum_Ins_L_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Insula_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Ins_R_imp <- lmer(
  drinks_sum ~ Insula_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Ins_R_imp) 
check_singularity(drinks_sum_Ins_R_imp) 
check_outliers(drinks_sum_Ins_R_imp) 
vif(drinks_sum_Ins_R_imp) 
check_model(drinks_sum_Ins_R_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Ins_R_imp)

# - follow-up with robust SE
drinks_sum_Ins_R_imp_robust <- robust_mixed(drinks_sum_Ins_R_imp, type = 'CR2')
drinks_sum_Ins_R_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Ins_R_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_R_Baseline:MedNAC', 'Insula_R_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_R x Med',
    term == 'Insula_R_Baseline:MedNAC' ~ 'Interaction: Insula_R x Med',
    term == 'Insula_R_Baseline' ~ 'ROI: Insula (R)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>%  
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Ins_R_imp, drinks_sum_Ins_R_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Nacc_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Nac_L_imp <- lmer(
  drinks_sum ~ Nacc_L_Baseline*Med +
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Nac_L_imp)
check_singularity(drinks_sum_Nac_L_imp) 
check_outliers(drinks_sum_Nac_L_imp) 
vif(drinks_sum_Nac_L_imp) 
check_model(drinks_sum_Nac_L_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Nac_L_imp) 

# - follow-up with robust SE
drinks_sum_Nac_L_imp_robust <- robust_mixed(drinks_sum_Nac_L_imp, type = 'CR2')
drinks_sum_Nac_L_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Nac_L_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_L_Baseline:MedNAC', 'Nacc_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc L x Med',
    term == 'Nacc_L_Baseline:MedNAC' ~ 'Interaction: Nacc_L_Baseline x Med',
    term == 'Nacc_L_Baseline' ~ 'ROI: Nacc (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Nac_L_imp, drinks_sum_Nac_L_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#              primary predictor: baseline Nacc_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Nac_R_imp <- lmer(
  drinks_sum ~ Nacc_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Nac_R_imp) 
check_singularity(drinks_sum_Nac_R_imp) 
check_outliers(drinks_sum_Nac_R_imp) 
vif(drinks_sum_Nac_R_imp) 
check_model(drinks_sum_Nac_R_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Nac_R_imp) 

# - follow-up with robust SE
drinks_sum_Nac_R_imp_robust <- robust_mixed(drinks_sum_Nac_R_imp, type = 'CR2')
drinks_sum_Nac_R_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Nac_R_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_R_Baseline:MedNAC', 'Nacc_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_R x Med',
    term == 'Nacc_R_Baseline:MedNAC' ~ 'Interaction: Nacc_R x Med',
    term == 'Nacc_R_Baseline' ~ 'ROI: Nacc (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Nac_R_imp, drinks_sum_Nac_R_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Putamen_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Put_L_imp <- lmer(
  drinks_sum ~ Putamen_L_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Put_L_imp) 
check_singularity(drinks_sum_Put_L_imp) 
check_outliers(drinks_sum_Put_L_imp) 
vif(drinks_sum_Put_L_imp) 
check_model(drinks_sum_Put_L_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Put_L_imp) 

# - follow-up with robust SE
drinks_sum_Nac_L_imp_robust <- robust_mixed(drinks_sum_Put_L_imp, type = 'CR2')
drinks_sum_Nac_L_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Nac_L_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_L_Baseline:MedNAC', 'Putamen_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_L x Med',
    term == 'Putamen_L_Baseline:MedNAC' ~ 'Interaction: Putamen_L x Med',
    term == 'Putamen_L_Baseline' ~ 'ROI: Putamen (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Put_L_imp, drinks_sum_Nac_L_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Putamen_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_Put_R_imp <- lmer(
  drinks_sum ~ Putamen_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_Put_R_imp) 
check_singularity(drinks_sum_Put_R_imp) 
check_outliers(drinks_sum_Put_R_imp) 
vif(drinks_sum_Put_R_imp) 
check_model(drinks_sum_Put_R_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_Put_R_imp)

# - follow-up: drop age
temp <- lmer(
  drinks_sum ~ Putamen_R_Baseline*Med + 
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + race +
    (1 | record_id),
  df_imp_2l)
summary(drinks_sum_Put_R_imp)
summary(temp)
rm(temp)

# - follow-up with robust SE
drinks_sum_Put_R_imp_robust <- robust_mixed(drinks_sum_Put_R_imp, type = 'CR2')
drinks_sum_Put_R_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_Put_R_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_R_Baseline:MedNAC', 'Putamen_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_R x Med',
    term == 'Putamen_R_Baseline:MedNAC' ~ 'Interaction: Putamen_R x Med',
    term == 'Putamen_R_Baseline' ~ 'ROI: Putamen (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>%  
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_Put_R_imp, drinks_sum_Put_R_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline AOCDS*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_AOCDS_imp <- lmer(
  drinks_sum ~ AOCDS_Baseline*Med +
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_AOCDS_imp) 
check_singularity(drinks_sum_AOCDS_imp) 
check_outliers(drinks_sum_AOCDS_imp)
vif(drinks_sum_AOCDS_imp)
check_model(drinks_sum_AOCDS_imp, check = 'all')
check_heteroscedasticity(drinks_sum_AOCDS_imp) 

# - follow-up with robust SE
drinks_sum_AOCDS_imp_robust <- robust_mixed(drinks_sum_AOCDS_imp, type = 'CR2')
drinks_sum_AOCDS_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_AOCDS_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'AOCDS_Baseline:MedNAC', 'AOCDS_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinks_sum_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: AOCDS x Med',
    term == 'AOCDS_Baseline:MedNAC' ~ 'Interaction: AOCDS x Med',
    term == 'AOCDS_Baseline' ~ 'AOCDS (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_AOCDS_imp, drinks_sum_AOCDS_imp_robust, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline ACQ*Med
#------------------------------------------------------------------------------#

# - initial model
drinks_sum_ACQ_imp <- lmer(
  drinks_sum ~ ACQ_Baseline*Med +
    # covariates
    Visit + Seq + drinks_sum_base + cannabis_days_base + nicotine_days_base + 
    sex + age + race +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinks_sum_ACQ_imp) 
check_singularity(drinks_sum_ACQ_imp)
check_outliers(drinks_sum_ACQ_imp) 
vif(drinks_sum_ACQ_imp)
check_model(drinks_sum_ACQ_imp, check = 'all') 
check_heteroscedasticity(drinks_sum_ACQ_imp) 

# - follow-up with robust SE
drinks_sum_ACQ_imp_robust <- robust_mixed(drinks_sum_ACQ_imp, type = 'CR2')
drinks_sum_ACQ_imp_robust

# - save model object
temp <- broom::tidy(drinks_sum_ACQ_imp_robust) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-mb.se, -statistic, -stars) %>% 
  rename('statistic' = 't.stat') %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'ACQ_Baseline:MedNAC', 'ACQ_Baseline', 'MedNAC', 'VisitVisit_2', 
  'SeqA', 'drinks_sum_base', 'cannabis_days_base', 'nicotine_days_base',  
  'sexFemale', 'age', 'raceAsian')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACQ x Med',
    term == 'ACQ_Baseline:MedNAC' ~ 'Interaction: ACQ x Med',
    term == 'ACQ_Baseline' ~ 'ACQ (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinks_sum_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex',
    term == 'age' ~ 'Age',
    term == 'raceAsian' ~ 'Race')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinks_sum_summary <- rbind(drinks_sum_summary, temp)
rm(drinks_sum_ACQ_imp, drinks_sum_ACQ_imp_robust, temp, row_order)

### export final results

# - update if value is 0, change to <0.01

drinks_sum_summary <- drinks_sum_summary %>% 
  mutate(
    Estimate = ifelse(Estimate == 0.00, '<0.01', Estimate),
    SE = ifelse(SE == 0.00, '<0.01', SE),
    `t-statistic` = ifelse(`t-statistic` == 0.00, '<0.01', `t-statistic`),
    p = ifelse(p == 0.00, '<0.01', p))

write.csv(drinks_sum_summary, 
          'output/models_exploratory/TLFB/drinks_sum_summary.csv')
rm(drinks_sum_summary)


# Exploratory TLFB: Drinking Days ----------------------------------------------

#------------------------------------------------------------------------------#
#                 primary predictor: baseline ACC_Mid*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_ACC_imp <- lmer(
  drinking_days ~ ACC_Mid_Baseline*Med + 
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_ACC_imp) 
check_singularity(drinking_days_ACC_imp)
check_outliers(drinking_days_ACC_imp) 
vif(drinking_days_ACC_imp) 
check_model(drinking_days_ACC_imp, check = 'all') 
check_heteroscedasticity(drinking_days_ACC_imp)

# - save model object 
drinking_days_summary <- broom::tidy(drinking_days_ACC_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 
names(drinking_days_summary)

row_order <- c(
  'Model', 'ACC_Mid_Baseline:MedNAC', 'ACC_Mid_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

drinking_days_summary <- drinking_days_summary %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline:MedNAC' ~ 'Interaction: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline' ~ 'ROI: ACC_Mid (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

rm(drinking_days_ACC_imp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Amygdala_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Amy_L_imp <- lmer(
  drinking_days ~ Amygdala_L_Baseline*Med +  
    # covariates 
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Amy_L_imp) 
check_singularity(drinking_days_Amy_L_imp) 
check_outliers(drinking_days_Amy_L_imp) 
vif(drinking_days_Amy_L_imp) 
check_model(drinking_days_Amy_L_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Amy_L_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Amy_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_L_Baseline:MedNAC', 'Amygdala_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline:MedNAC' ~ 'Interaction: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline' ~ 'ROI: Amygdala (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Amy_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Amygdala_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Amy_R_imp <- lmer(
  drinking_days ~ Amygdala_R_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Amy_R_imp) 
check_singularity(drinking_days_Amy_R_imp)
check_outliers(drinking_days_Amy_R_imp) 
vif(drinking_days_Amy_R_imp)
check_model(drinking_days_Amy_R_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Amy_R_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Amy_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_R_Baseline:MedNAC', 'Amygdala_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline:MedNAC' ~ 'Interaction: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline' ~ 'ROI: Amygdala (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Amy_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Caudate_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Cau_L_imp <- lmer(
  drinking_days ~ Caudate_L_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Cau_L_imp) 
check_singularity(drinking_days_Cau_L_imp) 
check_outliers(drinking_days_Cau_L_imp) 
vif(drinking_days_Cau_L_imp) 
check_model(drinking_days_Cau_L_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Cau_L_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Cau_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_L_Baseline:MedNAC', 'Caudate_L', 'MedNAC', 'VisitVisit_2',
  'SeqA', 'drinking_days_base', 'cannabis_days_base', 'nicotine_days_base', 
  'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_L x Med',
    term == 'Caudate_L_Baseline:MedNAC' ~ 'Interaction: Caudate_L x Med',
    term == 'Caudate_L_Baseline' ~ 'ROI: Caudate (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Cau_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Caudate_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Cau_R_imp <- lmer(
  drinking_days ~ Caudate_R_Baseline*Med +  
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Cau_R_imp) 
check_singularity(drinking_days_Cau_R_imp) 
check_outliers(drinking_days_Cau_R_imp) 
vif(drinking_days_Cau_R_imp) 
check_model(drinking_days_Cau_R_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Cau_R_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Cau_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_R_Baseline:MedNAC', 'Caudate_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_R x Med',
    term == 'Caudate_R_Baseline:MedNAC' ~ 'Interaction: Caudate_R x Med',
    term == 'Caudate_R_Baseline' ~ 'ROI: Caudate (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Cau_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Insula_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Ins_L_imp <- lmer(
  drinking_days ~ Insula_L_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Ins_L_imp) 
check_singularity(drinking_days_Ins_L_imp) 
check_outliers(drinking_days_Ins_L_imp) 
vif(drinking_days_Ins_L_imp) 
check_model(drinking_days_Ins_L_imp, check = 'all')
check_heteroscedasticity(drinking_days_Ins_L_imp)

# - save model object
temp <- broom::tidy(drinking_days_Ins_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_L_Baseline:MedNAC', 'Insula_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_L x Med', 
    term == 'Insula_L_Baseline:MedNAC' ~ 'Interaction: Insula_L x Med',
    term == 'Insula_L_Baseline' ~ 'ROI: Insula (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Ins_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Insula_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Ins_R_imp <- lmer(
  drinking_days ~ Insula_R_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Ins_R_imp) 
check_singularity(drinking_days_Ins_R_imp) 
check_outliers(drinking_days_Ins_R_imp) 
vif(drinking_days_Ins_R_imp) 
check_model(drinking_days_Ins_R_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Ins_R_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Ins_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_R_Baseline:MedNAC', 'Insula_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_R x Med',
    term == 'Insula_R_Baseline:MedNAC' ~ 'Interaction: Insula_R x Med',
    term == 'Insula_R_Baseline' ~ 'ROI: Insula (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Ins_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#             primary predictor: baseline Nacc_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Nac_L_imp <- lmer(
  drinking_days ~ Nacc_L_Baseline*Med + 
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Nac_L_imp) 
check_singularity(drinking_days_Nac_L_imp) 
check_outliers(drinking_days_Nac_L_imp) 
vif(drinking_days_Nac_L_imp) 
check_model(drinking_days_Nac_L_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Nac_L_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Nac_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_L_Baseline:MedNAC', 'Nacc_L_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base',
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_L x Med',
    term == 'Nacc_L_Baseline:MedNAC' ~ 'Interaction: Nacc_L x Med',
    term == 'Nacc_L_Baseline' ~ 'ROI: Nacc (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Nac_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Nacc_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Nac_R_imp <- lmer(
  drinking_days ~ Nacc_R_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Nac_R_imp) 
check_singularity(drinking_days_Nac_R_imp) 
check_outliers(drinking_days_Nac_R_imp)
vif(drinking_days_Nac_R_imp) 
check_model(drinking_days_Nac_R_imp, check = 'all') 
check_heteroscedasticity(drinking_days_Nac_R_imp) 

# - save model object
temp <- broom::tidy(drinking_days_Nac_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_R_Baseline:MedNAC', 'Nacc_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA',  'drinking_days_base', 'cannabis_days_base',
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_R x Med',
    term == 'Nacc_R_Baseline:MedNAC' ~ 'Interaction: Nacc_R x Med',
    term == 'Nacc_R_Baseline' ~ 'ROI: Nacc (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Nac_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Putamen_L*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Put_L_imp <- lmer(
  drinking_days ~ Putamen_L_Baseline*Med + 
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Put_L_imp) 
check_singularity(drinking_days_Put_L_imp) 
check_outliers(drinking_days_Put_L_imp) 
vif(drinking_days_Put_L_imp) 
check_model(drinking_days_Put_L_imp, check = 'all')
check_heteroscedasticity(drinking_days_Put_L_imp)

# - save model object
temp <- broom::tidy(drinking_days_Put_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_L_Baseline:MedNAC', 'Putamen_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_L x Med',
    term == 'Putamen_L_Baseline:MedNAC' ~ 'Interaction: Putamen_L_Baseline x Med',
    term == 'Putamen_L_Baseline' ~ 'ROI: Putamen (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Put_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Putamen_R*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_Put_R_imp <- lmer(
  drinking_days ~ Putamen_R_Baseline*Med +
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_Put_R_imp) 
check_singularity(drinking_days_Put_R_imp) 
check_outliers(drinking_days_Put_R_imp) 
vif(drinking_days_Put_R_imp) 
check_model(drinking_days_Put_R_imp, check = 'all')
check_heteroscedasticity(drinking_days_Put_R_imp)

# - save model object
temp <- broom::tidy(drinking_days_Put_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_R_Baseline:MedNAC', 'Putamen_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'drinking_days_base', 'cannabis_days_base', 
  'nicotine_days_base', 'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_R x Med',
    term == 'Putamen_R_Baseline:MedNAC' ~ 'Interaction: Putamen_R x Med',
    term == 'Putamen_R_Baseline' ~ 'ROI: Putamen (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_Put_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline AOCDS*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_AOCDS_imp <- lmer(
  drinking_days ~ AOCDS_Baseline*Med + 
    # covaraites
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_AOCDS_imp) 
check_singularity(drinking_days_AOCDS_imp) 
check_outliers(drinking_days_AOCDS_imp) 
vif(drinking_days_AOCDS_imp) 
check_model(drinking_days_AOCDS_imp, check = 'all') 
check_heteroscedasticity(drinking_days_AOCDS_imp) 

# - save model object
temp <- broom::tidy(drinking_days_AOCDS_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'AOCDS_Baseline:MedNAC', 'AOCDS_Baseline', 'MedNAC', 'VisitVisit_2', 
  'SeqA', 'drinking_days_base', 'cannabis_days_base', 'nicotine_days_base', 
  'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: AOCDS x Med',
    term == 'AOCDS_Baseline:MedNAC' ~ 'Interaction: AOCDS x Med',
    term == 'AOCDS_Baseline' ~ 'AOCDS (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_AOCDS_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline ACQ*Med
#------------------------------------------------------------------------------#

# - initial model
drinking_days_ACQ_imp <- lmer(
  drinking_days ~ ACQ_Baseline*Med + 
    # covariates
    Visit + Seq + drinking_days_base + cannabis_days_base + nicotine_days_base + 
    sex +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(drinking_days_ACQ_imp) 
check_singularity(drinking_days_ACQ_imp) 
check_outliers(drinking_days_ACQ_imp) 
vif(drinking_days_ACQ_imp) 
check_model(drinking_days_ACQ_imp, check = 'all') 
check_heteroscedasticity(drinking_days_ACQ_imp) 

# - save model object
temp <- broom::tidy(drinking_days_ACQ_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'ACQ_Baseline:MedNAC', 'ACQ_Baseline', 'MedNAC', 'VisitVisit_2', 
  'SeqA', 'drinking_days_base', 'cannabis_days_base', 'nicotine_days_base', 
  'sexFemale')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACQ x Med',
    term == 'ACQ_Baseline:MedNAC' ~ 'Interaction: ACQ x Med',
    term == 'ACQ_Baseline' ~ 'ACQ (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'drinking_days_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)',
    term == 'sexFemale' ~ 'Sex')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

drinking_days_summary <- rbind(drinking_days_summary, temp)
rm(drinking_days_ACQ_imp, temp, row_order)

### export final results

# - update if value is 0, change to <0.01

drinking_days_summary <- drinking_days_summary %>% 
  mutate(
    Estimate = ifelse(Estimate == 0.00, '<0.01', Estimate),
    SE = ifelse(SE == 0.00, '<0.01', SE),
    `t-statistic` = ifelse(`t-statistic` == 0.00, '<0.01', `t-statistic`),
    p = ifelse(p == 0.00, '<0.01', p))

write.csv(drinking_days_summary, 
          'output/models_exploratory/TLFB/drinking_days_summary.csv')
rm(drinking_days_summary)


# Exploratory TLFB: Binge Drinking Days -----------------------------------

#------------------------------------------------------------------------------#
#               primary predictor: baseline ACC_Mid*Med
#------------------------------------------------------------------------------#

# - initial model
binge_ACC_imp <- lmer(
  binge ~ ACC_Mid_Baseline*Med + 
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_ACC_imp) 
check_singularity(binge_ACC_imp)
check_outliers(binge_ACC_imp) 
vif(binge_ACC_imp) 
check_model(binge_ACC_imp, check = 'all') 
check_heteroscedasticity(binge_ACC_imp)

# - save model object 
binge_summary <- broom::tidy(binge_ACC_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 
names(binge_summary)

row_order <- c(
  'Model', 'ACC_Mid_Baseline:MedNAC', 'ACC_Mid_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

binge_summary <- binge_summary %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline:MedNAC' ~ 'Interaction: ACC_Mid x Med',
    term == 'ACC_Mid_Baseline' ~ 'ROI: ACC_Mid (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

rm(binge_ACC_imp)

#------------------------------------------------------------------------------#
#              primary predictor: baseline Amygdala_L*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Amy_L_imp <- lmer(
  binge ~ Amygdala_L_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Amy_L_imp) 
check_singularity(binge_Amy_L_imp) 
check_outliers(binge_Amy_L_imp) 
vif(binge_Amy_L_imp) 
check_model(binge_Amy_L_imp, check = 'all') 
check_heteroscedasticity(binge_Amy_L_imp)  

# - save model object
temp <- broom::tidy(binge_Amy_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_L_Baseline:MedNAC', 'Amygdala_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline:MedNAC' ~ 'Interaction: Amygdala_L x Med',
    term == 'Amygdala_L_Baseline' ~ 'ROI: Amygdala (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Amy_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Amygdala_R*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Amy_R_imp <- lmer(
  binge ~ Amygdala_R_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Amy_R_imp) 
check_singularity(binge_Amy_R_imp)
check_outliers(binge_Amy_R_imp)
vif(binge_Amy_R_imp)
check_model(binge_Amy_R_imp, check = 'all')
check_heteroscedasticity(binge_Amy_R_imp) 

# - save model object
temp <- broom::tidy(binge_Amy_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Amygdala_R_Baseline:MedNAC', 'Amygdala_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline:MedNAC' ~ 'Interaction: Amygdala_R x Med',
    term == 'Amygdala_R_Baseline' ~ 'ROI: Amygdala (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Amy_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Caudate_L*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Cau_L_imp <- lmer(
  binge ~ Caudate_L_Baseline*Med + 
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Cau_L_imp) 
check_singularity(binge_Cau_L_imp) 
check_outliers(binge_Cau_L_imp)
vif(binge_Cau_L_imp) 
check_model(binge_Cau_L_imp, check = 'all') 
check_heteroscedasticity(binge_Cau_L_imp) 

# - save model object
temp <- broom::tidy(binge_Cau_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_L_Baseline:MedNAC', 'Caudate_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_L x Med',
    term == 'Caudate_L_Baseline:MedNAC' ~ 'Interaction: Caudate_L x Med',
    term == 'Caudate_L_Baseline' ~ 'ROI: Caudate (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Cau_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Caudate_R*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Cau_R_imp <- lmer(
  binge ~ Caudate_R_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Cau_R_imp) 
check_singularity(binge_Cau_R_imp)
check_outliers(binge_Cau_R_imp) 
vif(binge_Cau_R_imp) 
check_model(binge_Cau_R_imp, check = 'all') 
check_heteroscedasticity(binge_Cau_R_imp)

# - save model object
temp <- broom::tidy(binge_Cau_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Caudate_R_Baseline:MedNAC', 'Caudate_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Caudate_R x Med',
    term == 'Caudate_R_Baseline:MedNAC' ~ 'Interaction: Caudate_R x Med',
    term == 'Caudate_R_Baseline' ~ 'ROI: Caudate (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Cau_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Insula_L*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Ins_L_imp <- lmer(
  binge ~ Insula_L_Baseline*Med + 
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Ins_L_imp) 
check_singularity(binge_Ins_L_imp) 
check_outliers(binge_Ins_L_imp) 
vif(binge_Ins_L_imp) 
check_model(binge_Ins_L_imp, check = 'all') 
check_heteroscedasticity(binge_Ins_L_imp) 

# - save model object
temp <- broom::tidy(binge_Ins_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_L_Baseline:MedNAC', 'Insula_L_Baseline', 'MedNAC',
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_L x Med',
    term == 'Insula_L_Baseline:MedNAC' ~ 'Interaction: Insula_L x Med',
    term == 'Insula_L_Baseline' ~ 'ROI: Insula (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Ins_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#             primary predictor: baseline Insula_R*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Ins_R_imp <- lmer(
  binge ~ Insula_R_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Ins_R_imp)
check_singularity(binge_Ins_R_imp) 
check_outliers(binge_Ins_R_imp)
vif(binge_Ins_R_imp) 
check_model(binge_Ins_R_imp, check = 'all') 
check_heteroscedasticity(binge_Ins_R_imp)

# - save model object
temp <- broom::tidy(binge_Ins_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Insula_R_Baseline:MedNAC', 'Insula_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Insula_R x Med',
    term == 'Insula_R_Baseline:MedNAC' ~ 'Interaction: Insula_R x Med',
    term == 'Insula_R_Baseline' ~ 'ROI: Insula (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Ins_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Nacc_L*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Nac_L_imp <- lmer(
  binge ~ Nacc_L_Baseline*Med + 
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Nac_L_imp) 
check_singularity(binge_Nac_L_imp) 
check_outliers(binge_Nac_L_imp) 
vif(binge_Nac_L_imp) 
check_model(binge_Nac_L_imp, check = 'all') 
check_heteroscedasticity(binge_Nac_L_imp) 

# - save model object
temp <- broom::tidy(binge_Nac_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_L_Baseline:MedNAC', 'Nacc_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base',
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_L x Med',
    term == 'Nacc_L_Baseline:MedNAC' ~ 'Interaction: Nacc_L x Med',
    term == 'Nacc_L_Baseline' ~ 'ROI: Nacc (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Nac_L_imp, temp, row_order)

#------------------------------------------------------------------------------#
#               primary predictor: baseline Nacc_R*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Nac_R_imp <- lmer(
  binge ~ Nacc_R_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Nac_R_imp) 
check_singularity(binge_Nac_R_imp) 
check_outliers(binge_Nac_R_imp) 
vif(binge_Nac_R_imp)
check_model(binge_Nac_R_imp, check = 'all') 
check_heteroscedasticity(binge_Nac_R_imp) 

# - save model object
temp <- broom::tidy(binge_Nac_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Nacc_R_Baseline:MedNAC', 'Nacc_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Nacc_R x Med',
    term == 'Nacc_R_Baseline:MedNAC' ~ 'Interaction: Nacc_R x Med',
    term == 'Nacc_R_Baseline' ~ 'ROI: Nacc (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Nac_R_imp, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline Putamen_L*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Put_L_imp <- lmer(
  binge ~ Putamen_L_Baseline*Med +
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Put_L_imp) 
check_singularity(binge_Put_L_imp) 
check_outliers(binge_Put_L_imp) 
vif(binge_Put_L_imp) 
check_model(binge_Put_L_imp, check = 'all') 
check_heteroscedasticity(binge_Put_L_imp) 

# - save model object (including outlier)
temp <- broom::tidy(binge_Put_L_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>% 
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_L_Baseline:MedNAC', 'Putamen_L_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_L x Med',
    term == 'Putamen_L_Baseline:MedNAC' ~ 'Interaction: Putamen_L x Med',
    term == 'Putamen_L_Baseline' ~ 'ROI: Putamen (L) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Put_L_imp, binge_Put_L_imp_out, temp, row_order)

#------------------------------------------------------------------------------#
#                 primary predictor: baseline Putamen_R*Med
#------------------------------------------------------------------------------#

# - initial model
binge_Put_R_imp <- lmer(
  binge ~ Putamen_R_Baseline*Med +  
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base +
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_Put_R_imp) 
check_singularity(binge_Put_R_imp) 
check_outliers(binge_Put_R_imp) 
vif(binge_Put_R_imp) 
check_model(binge_Put_R_imp, check = 'all') 
check_heteroscedasticity(binge_Put_R_imp) 

# - save model object
temp <- broom::tidy(binge_Put_R_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'Putamen_R_Baseline:MedNAC', 'Putamen_R_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: Putamen_R x Med',
    term == 'Putamen_R_Baseline:MedNAC' ~ 'Interaction: Putamen_R x Med',
    term == 'Putamen_R_Baseline' ~ 'ROI: Putamen (R) (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_Put_R_imp, binge_Put_R_imp_out, temp, row_order)

#------------------------------------------------------------------------------#
#                primary predictor: baseline AOCDS*Med
#------------------------------------------------------------------------------#

# - initial model
binge_AOCDS_imp <- lmer(
  binge ~ AOCDS_Baseline*Med + 
    # covaraites
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_AOCDS_imp) 
check_singularity(binge_AOCDS_imp) 
check_outliers(binge_AOCDS_imp) 
vif(binge_AOCDS_imp) 
check_model(binge_AOCDS_imp, check = 'all') 
check_heteroscedasticity(binge_AOCDS_imp)

# - save model object
temp <- broom::tidy(binge_AOCDS_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'AOCDS_Baseline:MedNAC', 'AOCDS_Baseline', 'MedNAC', 
  'VisitVisit_2', 'SeqA', 'binge_base', 'cannabis_days_base', 
  'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: AOCDS x Med',
    term == 'AOCDS_Baseline:MedNAC' ~ 'Interaction: AOCDS x Med',
    term == 'AOCDS_Baseline' ~ 'AOCDS (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

# - follow-up on interaction
temp_2 <- df_imp_2l %>% 
  group_by(Med) %>% 
  summarise(
    mean_binge = mean(binge),
    sd_binge = sd(binge))

# figure

df_fig <- df_imp_2l %>% 
  mutate(
    Medication = case_when(
      Med == 'NAC' ~ 'N-acetylcysteine',
      Med == 'Placebo' ~ 'Placebo'))

ggplot(data = df_fig, 
       aes(fill = Medication, x = AOCDS_Baseline, y = dpdd, color = Medication)) +
  geom_smooth(method='lm', se = FALSE, formula= y~x) +
  theme_classic(base_size = 18) +
  labs(x ="AOCDS", y = "Binge Drinking Days", 
       family = "Arial") +
  theme(
    legend.text = element_text(family = "Arial"),
    legend.title = element_text(family = "Arial"), 
    axis.title.x = element_text(size=20, face="bold", family = "Arial"),
    axis.title.y = element_text(size=20, face="bold", family = "Arial"),
    axis.text = element_text(color="black", size=14, family = "Arial")) 

binge_summary <- rbind(binge_summary, temp)
rm(binge_AOCDS_imp, temp, temp_2, row_order, df_fig)

#------------------------------------------------------------------------------#
#              primary predictor: baseline ACQ*Med
#------------------------------------------------------------------------------#

# - initial model
binge_ACQ_imp <- lmer(
  binge ~ ACQ_Baseline*Med + 
    # covariates
    Visit + Seq + binge_base + cannabis_days_base + nicotine_days_base + 
    (1 | record_id),
  df_imp_2l)

# - model performance check
testDispersion(binge_ACQ_imp)
check_singularity(binge_ACQ_imp) 
check_outliers(binge_ACQ_imp)
vif(binge_ACQ_imp) 
check_model(binge_ACQ_imp, check = 'all') 
check_heteroscedasticity(binge_ACQ_imp) 

# - save model object
temp <- broom::tidy(binge_ACQ_imp) %>%
  mutate_if(is.numeric, round, 2) %>% 
  select(-effect, -group, -df) %>% 
  filter(row_number() <= n()-2) %>% 
  slice(-1) %>%
  add_row(term = 'Model', estimate = NA, std.error = NA, 
          statistic = NA, p.value = NA) 

row_order <- c(
  'Model', 'ACQ_Baseline:MedNAC', 'ACQ_Baseline', 'MedNAC', 'VisitVisit_2', 
  'SeqA', 'binge_base', 'cannabis_days_base', 'nicotine_days_base')

temp <- temp %>%
  slice(match(row_order, term)) %>%   
  mutate(term = case_when(
    term == 'Model' ~ 'Model: ACQ x Med',
    term == 'ACQ_Baseline:MedNAC' ~ 'Interaction: ACQ x Med',
    term == 'ACQ_Baseline' ~ 'ACQ (baseline)',
    term == 'MedNAC' ~ 'Medication',
    term == 'VisitVisit_2' ~ 'Visit',
    term == 'SeqA' ~ 'Sequence',
    term == 'binge_base' ~ 'Alcohol Use Outcome (baseline)',
    term == 'cannabis_days_base' ~ 'Cannabis Use Days (baseline)',
    term == 'nicotine_days_base' ~ 'Nicotine Use Days (baseline)')) %>% 
  rename('Fixed Effect' = term,
         'Estimate' = estimate,
         'SE' = std.error,
         't-statistic' = statistic,
         'p' = p.value)

binge_summary <- rbind(binge_summary, temp)
rm(binge_ACQ_imp, binge_ACQ_imp, temp, row_order)

### export final results

# - update if value is 0, change to <0.01

binge_summary <- binge_summary %>% 
  mutate(
    Estimate = ifelse(Estimate == 0.00, '<0.01', Estimate),
    SE = ifelse(SE == 0.00, '<0.01', SE),
    `t-statistic` = ifelse(`t-statistic` == 0.00, '<0.01', `t-statistic`),
    p = ifelse(p == 0.00, '<0.01', p))

write.csv(binge_summary, 'output/models_exploratory/TLFB/binge_summary.csv')
rm(binge_summary)


