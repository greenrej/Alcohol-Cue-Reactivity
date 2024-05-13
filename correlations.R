# Correlations: ROI & Subjective Craving ----------------------------------

# correlation between ROI & subjective craving in each medication condition

# subset dataframes by medication condition
df_NAC <- df %>% 
  subset(Med == 'NAC') %>% 
  select(ACC_Mid, ends_with('L'), ends_with('R'), AOCDS, ACQ)

df_Placebo <- df %>% 
  subset(Med == 'Placebo') %>% 
  select(ACC_Mid, ends_with('L'), ends_with('R'), AOCDS, ACQ)

# correlation (NAC): AOCDS & 11 ROIs 
a1 <- cor.test(df_NAC$AOCDS, df_NAC$ACC_Mid)
a2 <- cor.test(df_NAC$AOCDS, df_NAC$Amygdala_L)
a3 <- cor.test(df_NAC$AOCDS, df_NAC$Amygdala_R)
a4 <- cor.test(df_NAC$AOCDS, df_NAC$Caudate_L)
a5 <- cor.test(df_NAC$AOCDS, df_NAC$Caudate_R)
a6 <- cor.test(df_NAC$AOCDS, df_NAC$Insula_L)
a7 <- cor.test(df_NAC$AOCDS, df_NAC$Insula_R)
a8 <- cor.test(df_NAC$AOCDS, df_NAC$Nacc_L)
a9 <- cor.test(df_NAC$AOCDS, df_NAC$Nacc_R)
a10 <- cor.test(df_NAC$AOCDS, df_NAC$Putamen_L)
a11 <- cor.test(df_NAC$AOCDS, df_NAC$Putamen_R)

# correlation (NAC): ACQ & 11 ROIs 
b1 <- cor.test(df_NAC$ACQ, df_NAC$ACC_Mid)
b2 <- cor.test(df_NAC$ACQ, df_NAC$Amygdala_L)
b3 <- cor.test(df_NAC$ACQ, df_NAC$Amygdala_R)
b4 <- cor.test(df_NAC$ACQ, df_NAC$Caudate_L)
b5 <- cor.test(df_NAC$ACQ, df_NAC$Caudate_R)
b6 <- cor.test(df_NAC$ACQ, df_NAC$Insula_L)
b7 <- cor.test(df_NAC$ACQ, df_NAC$Insula_R)
b8 <- cor.test(df_NAC$ACQ, df_NAC$Nacc_L)
b9 <- cor.test(df_NAC$ACQ, df_NAC$Nacc_R)
b10 <- cor.test(df_NAC$ACQ, df_NAC$Putamen_L)
b11 <- cor.test(df_NAC$ACQ, df_NAC$Putamen_R)

# correlation (Placebo): AOCDS & 11 ROIs 
c1 <- cor.test(df_Placebo$AOCDS, df_Placebo$ACC_Mid)
c2 <- cor.test(df_Placebo$AOCDS, df_Placebo$Amygdala_L)
c3 <- cor.test(df_Placebo$AOCDS, df_Placebo$Amygdala_R)
c4 <- cor.test(df_Placebo$AOCDS, df_Placebo$Caudate_L)
c5 <- cor.test(df_Placebo$AOCDS, df_Placebo$Caudate_R)
c6 <- cor.test(df_Placebo$AOCDS, df_Placebo$Insula_L)
c7 <- cor.test(df_Placebo$AOCDS, df_Placebo$Insula_R)
c8 <- cor.test(df_Placebo$AOCDS, df_Placebo$Nacc_L)
c9 <- cor.test(df_Placebo$AOCDS, df_Placebo$Nacc_R)
c10 <- cor.test(df_Placebo$AOCDS, df_Placebo$Putamen_L)
c11 <- cor.test(df_Placebo$AOCDS, df_Placebo$Putamen_R)

# correlation (Placebo): ACQ & 11 ROIs 
d1 <- cor.test(df_Placebo$ACQ, df_Placebo$ACC_Mid)
d2 <- cor.test(df_Placebo$ACQ, df_Placebo$Amygdala_L)
d3 <- cor.test(df_Placebo$ACQ, df_Placebo$Amygdala_R)
d4 <- cor.test(df_Placebo$ACQ, df_Placebo$Caudate_L)
d5 <- cor.test(df_Placebo$ACQ, df_Placebo$Caudate_R)
d6 <- cor.test(df_Placebo$ACQ, df_Placebo$Insula_L)
d7 <- cor.test(df_Placebo$ACQ, df_Placebo$Insula_R)
d8 <- cor.test(df_Placebo$ACQ, df_Placebo$Nacc_L)
d9 <- cor.test(df_Placebo$ACQ, df_Placebo$Nacc_R)
d10 <- cor.test(df_Placebo$ACQ, df_Placebo$Putamen_L)
d11 <- cor.test(df_Placebo$ACQ, df_Placebo$Putamen_R)

coef_a1 <- a1[["estimate"]][["cor"]]
coef_a2 <- a2[["estimate"]][["cor"]]
coef_a3 <- a3[["estimate"]][["cor"]]
coef_a4 <- a4[["estimate"]][["cor"]]
coef_a5 <- a5[["estimate"]][["cor"]]
coef_a6 <- a6[["estimate"]][["cor"]]
coef_a7 <- a7[["estimate"]][["cor"]]
coef_a8 <- a8[["estimate"]][["cor"]]
coef_a9 <- a9[["estimate"]][["cor"]]
coef_a10 <- a10[["estimate"]][["cor"]]
coef_a11 <- a11[["estimate"]][["cor"]]

coef_b1 <- b1[["estimate"]][["cor"]]
coef_b2 <- b2[["estimate"]][["cor"]]
coef_b3 <- b3[["estimate"]][["cor"]]
coef_b4 <- b4[["estimate"]][["cor"]]
coef_b5 <- b5[["estimate"]][["cor"]]
coef_b6 <- b6[["estimate"]][["cor"]]
coef_b7 <- b7[["estimate"]][["cor"]]
coef_b8 <- b8[["estimate"]][["cor"]]
coef_b9 <- b9[["estimate"]][["cor"]]
coef_b10 <- b10[["estimate"]][["cor"]]
coef_b11 <- b11[["estimate"]][["cor"]]

coef_c1 <- c1[["estimate"]][["cor"]]
coef_c2 <- c2[["estimate"]][["cor"]]
coef_c3 <- c3[["estimate"]][["cor"]]
coef_c4 <- c4[["estimate"]][["cor"]]
coef_c5 <- c5[["estimate"]][["cor"]]
coef_c6 <- c6[["estimate"]][["cor"]]
coef_c7 <- c7[["estimate"]][["cor"]]
coef_c8 <- c8[["estimate"]][["cor"]]
coef_c9 <- c9[["estimate"]][["cor"]]
coef_c10 <- c10[["estimate"]][["cor"]]
coef_c11 <- c11[["estimate"]][["cor"]]

coef_d1 <- d1[["estimate"]][["cor"]]
coef_d2 <- d2[["estimate"]][["cor"]]
coef_d3 <- d3[["estimate"]][["cor"]]
coef_d4 <- d4[["estimate"]][["cor"]]
coef_d5 <- d5[["estimate"]][["cor"]]
coef_d6 <- d6[["estimate"]][["cor"]]
coef_d7 <- d7[["estimate"]][["cor"]]
coef_d8 <- d8[["estimate"]][["cor"]]
coef_d9 <- d9[["estimate"]][["cor"]]
coef_d10 <- d10[["estimate"]][["cor"]]
coef_d11 <- d11[["estimate"]][["cor"]]

p_a1 <- a1[["p.value"]]
p_a2 <- a2[["p.value"]]
p_a3 <- a3[["p.value"]]
p_a4 <- a4[["p.value"]]
p_a5 <- a5[["p.value"]]
p_a6 <- a6[["p.value"]]
p_a7 <- a7[["p.value"]]
p_a8 <- a8[["p.value"]]
p_a9 <- a9[["p.value"]]
p_a10 <- a10[["p.value"]]
p_a11 <- a11[["p.value"]]

p_b1 <- b1[["p.value"]]
p_b2 <- b2[["p.value"]]
p_b3 <- b3[["p.value"]]
p_b4 <- b4[["p.value"]]
p_b5 <- b5[["p.value"]]
p_b6 <- b6[["p.value"]]
p_b7 <- b7[["p.value"]]
p_b8 <- b8[["p.value"]]
p_b9 <- b9[["p.value"]]
p_b10 <- b10[["p.value"]]
p_b11 <- b11[["p.value"]]

p_c1 <- c1[["p.value"]]
p_c2 <- c2[["p.value"]]
p_c3 <- c3[["p.value"]]
p_c4 <- c4[["p.value"]]
p_c5 <- c5[["p.value"]]
p_c6 <- c6[["p.value"]]
p_c7 <- c7[["p.value"]]
p_c8 <- c8[["p.value"]]
p_c9 <- c9[["p.value"]]
p_c10 <- c10[["p.value"]]
p_c11 <- c11[["p.value"]]

p_d1 <- d1[["p.value"]]
p_d2 <- d2[["p.value"]]
p_d3 <- d3[["p.value"]]
p_d4 <- d4[["p.value"]]
p_d5 <- d5[["p.value"]]
p_d6 <- d6[["p.value"]]
p_d7 <- d7[["p.value"]]
p_d8 <- d8[["p.value"]]
p_d9 <- d9[["p.value"]]
p_d10 <- d10[["p.value"]]
p_d11 <- d11[["p.value"]]

corr_NAC_AOCDS <- data.frame(
  'ROI' = c('ACC_Mid', 'Amygdala_L', 'Amygdala_R', 'Caudate_L', 'Caudate_R', 
            'Insula_L', 'Insula_R', 'Nacc_L', 'Nacc_R', 'Putamen_L', 
            'Putamen_R'),
  'coefficent' = c(coef_a1, coef_a2, coef_a3, coef_a4, coef_a5, coef_a6, 
                   coef_a7, coef_a8, coef_a9, coef_a10, coef_a11), 
  'p.value' = c(p_a1, p_a2, p_a3, p_a4, p_a5, p_a6, p_a7, p_a8, p_a9, 
                p_a10, p_a11)) %>% 
  mutate_if(is.numeric, round, 2)

corr_NAC_ACQ <- data.frame(
  'ROI' = c('ACC_Mid', 'Amygdala_L', 'Amygdala_R', 'Caudate_L', 'Caudate_R', 
            'Insula_L', 'Insula_R', 'Nacc_L', 'Nacc_R', 'Putamen_L', 
            'Putamen_R'),
  'coefficent' = c(coef_b1, coef_b2, coef_b3, coef_b4, coef_b5, coef_b6, 
                   coef_b7, coef_b8, coef_b9, coef_b10, coef_b11), 
  'p.value' = c(p_b1, p_b2, p_b3, p_b4, p_b5, p_b6, p_b7, p_b8, p_b9, 
                p_b10, p_b11)) %>% 
  mutate_if(is.numeric, round, 2)

corr_Placebo_AOCDS <- data.frame(
  'ROI' = c('ACC_Mid', 'Amygdala_L', 'Amygdala_R', 'Caudate_L', 'Caudate_R', 
            'Insula_L', 'Insula_R', 'Nacc_L', 'Nacc_R', 'Putamen_L', 
            'Putamen_R'),
  'coefficent' = c(coef_c1, coef_c2, coef_c3, coef_c4, coef_c5, coef_c6, 
                   coef_c7, coef_c8, coef_c9, coef_c10, coef_c11), 
  'p.value' = c(p_c1, p_c2, p_c3, p_c4, p_c5, p_c6, p_c7, p_c8, p_c9, 
                p_c10, p_c11)) %>% 
  mutate_if(is.numeric, round, 2)

corr_Placebo_ACQ <- data.frame(
  'ROI' = c('ACC_Mid', 'Amygdala_L', 'Amygdala_R', 'Caudate_L', 'Caudate_R', 
            'Insula_L', 'Insula_R', 'Nacc_L', 'Nacc_R', 'Putamen_L', 
            'Putamen_R'),
  'coefficent' = c(coef_d1, coef_d2, coef_d3, coef_d4, coef_d5, coef_d6, 
                   coef_d7, coef_d8, coef_d9, coef_d10, coef_d11), 
  'p.value' = c(p_d1, p_d2, p_d3, p_d4, p_d5, p_d6, p_d7, p_d8, p_d9, 
                p_d10, p_d11)) %>% 
  mutate_if(is.numeric, round, 2)

write.csv(corr_NAC_AOCDS, 'output/correlations/corr_NAC_AOCDS.csv')
write.csv(corr_NAC_ACQ, 'output/correlations/corr_NAC_ACQ.csv')
write.csv(corr_Placebo_AOCDS, 'output/correlations/corr_Placebo_AOCDS.csv')
write.csv(corr_Placebo_ACQ, 'output/correlations/corr_Placebo_ACQ.csv')

rm(df_NAC, df_Placebo,
   a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
   b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
   c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11,
   d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11,
   coef_a1, coef_a2, coef_a3, coef_a4, coef_a5, coef_a6, coef_a7, coef_a8, 
   coef_a9, coef_a10, coef_a11,
   coef_b1, coef_b2, coef_b3, coef_b4, coef_b5, coef_b6, coef_b7, coef_b8, 
   coef_b9, coef_b10, coef_b11,
   coef_c1, coef_c2, coef_c3, coef_c4, coef_c5, coef_c6, coef_c7, coef_c8, 
   coef_c9, coef_c10, coef_c11,
   coef_d1, coef_d2, coef_d3, coef_d4, coef_d5, coef_d6, coef_d7, coef_d8, 
   coef_d9, coef_d10, coef_d11,
   p_a1, p_a2, p_a3, p_a4, p_a5, p_a6, p_a7, p_a8, 
   p_a9, p_a10, p_a11,
   p_b1, p_b2, p_b3, p_b4, p_b5, p_b6, p_b7, p_b8, 
   p_b9, p_b10, p_b11,
   p_c1, p_c2, p_c3, p_c4, p_c5, p_c6, p_c7, p_c8, 
   p_c9, p_c10, p_c11,
   p_d1, p_d2, p_d3, p_d4, p_d5, p_d6, p_d7, p_d8, 
   p_d9, p_d10, p_d11,
   corr_NAC_AOCDS, corr_NAC_ACQ, corr_Placebo_AOCDS, corr_Placebo_ACQ)