#aov(RD_T ~ as.factor(teacherid), data = ecls)
summary(aov(RD_T ~ as.factor(teacherid), data = ecls))

#eta squared
57367/(57367 + 139256)

#random effect: we view the specific levels of the variable (e.g. teachers, classroms, etc.), as a random sample from a larger population. We wish to generalize to that population. 

#fixed effect: we do not wish to generalize beyond the specific levels of the variable that we have in our data

library(lme4)
library(lmerTest)
library(sjPlot)
library(dplyr)

library(brms)
#fit1 <- brm(formula = RD_T ~ SES + RD_T_pre + female +(SES + RD_T_pre + female | teacherid),data = ecls.nomiss)


#--------------------------------------------------------- centering

m1 <- lmer(data = ecls, RD_T ~ 1 + (1|teacherid), na.action = na.omit)

m2 <- lmer(data = ecls, RD_T ~ RD_T_pre + (1|teacherid), na.action = na.omit)


#grand mean centering
ecls$RD_T_pre.c <- ecls$RD_T_pre - mean(ecls$RD_T_pre, na.rm = T)
#look into tidyverse method with mutate

m3 <- lmer(data = ecls, RD_T ~ RD_T_pre.c + (1|teacherid), na.action = na.omit)

#group mean centering
#way easier with dplyr
ecls <- ecls %>%
  group_by(teacherid) %>% 
  mutate(RD_T_pre.gc = as.numeric(scale(RD_T_pre, center = T, scale = F)))


m4 <- lmer(data = ecls, RD_T ~ RD_T_pre.gc + (1|teacherid), na.action = na.omit)

m5 <- lmer(data = ecls,
           RD_T ~ RD_T_pre.gc + 
             (RD_T_pre.gc|teacherid), 
           na.action = na.omit)

m6 <- lmer(data = ecls, RD_T ~ RD_T_pre.gc + I(RD_T_pre_Mean - mean(RD_T_pre_Mean, na.rm = T)) + 
             (1|teacherid), 
           na.action = na.omit)

tab_model(m1, m4, m6, show.ci = F)

hist(ecls$RD_T_pre.gc)
