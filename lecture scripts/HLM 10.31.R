
library(haven)
library(vtable)
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)

alc <- read_sas("datasets/alcohol1_pp.sas7bdat")
vtable(alc)

alc$ID <- as.factor(alc$ID)

alc$AGE_c <- alc$AGE - mean(alc$AGE)

#lmerControl() used to adjust optimization type
#control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))

m0 <- lmer()

m2 <- lmer(data = alc, ALCUSE ~ AGE_14*COA + CPEER + (1 + AGE_14|ID),
           control = lmerControl(optimizer = "bobyqa",
                       optCtrl = list(maxfun = 200000)))

m3 <- lmer(data = alc, ALCUSE ~ AGE_14*COA + AGE_14*CPEER + (1 + AGE_14|ID),
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m2, m3)

m4 <- lmer(data = alc, ALCUSE ~ AGE_14*COA + AGE_14*CPEER + AGE_14*CPEER*COA + (1 + AGE_14|ID), 
           control = lmerControl(optimizer = "bobyqa", 
                                 optCtrl = list(maxfun = 200000)))
tab_model(m3, m4)

m5 <- lmer(data = alc, ALCUSE ~ AGE_14*CPEER*COA + MALE*COA + (1 + AGE_14|ID),
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m5)

#------------------------------------------------new dataset
tv <- read.csv("datasets/time_varying_3.csv", stringsAsFactors = F, header = T)
with(tv, table(id))
with(tv, table(time, id))

#vis
plot1 <-
  tv %>% ggplot(aes(x = time, y = Y, color = cut(X, breaks = 4))) + 
  geom_point(alpha = .5) + geom_smooth(span = 4) + theme_bw()

plot2 <-
  tv %>% ggplot(aes(x = time, y = Y)) + 
  geom_point(alpha = .5) + geom_smooth(span = 4) + theme_bw() +
  facet_wrap(~id)

plot3 <-
  tv %>% ggplot(aes(x = time, y = Y, color = cut(X, breaks = 4))) + 
  geom_point(alpha = .5) + theme_bw() +
  facet_wrap(~id)

#formal analysis
m0 <- lmer(data = tv, Y ~ time + (1 + time|id), 
           control = lmerControl(optimizer = "bobyqa",
                       optCtrl = list(maxfun = 200000)))
tab_model(m0)

m1 <- lmer(data = tv, Y ~ time + X + (1 + time|id), 
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m0, m1)

m2 <- lmer(data = tv, Y ~ time + X + (X + time|id), 
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
#can't add X to random effects; effect of X consistent across individuals
m3 <- lmer(data = tv, Y ~ time + X + W1 + (time|id), 
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m1, m3)

m4 <- lmer(data = tv, Y ~ time + X + W1 + W2 + (time|id), 
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m1, m3, m4)

#can add interactions, see previous example
#do not recommend adding time interaction with time-variant covariates; no longer a trajectory

#---------------------------------------------data reorganization
library(tidyr)
#pivot_wider() & pivot_longer()

tv$time_2 <- round(tv$time, 0)
with(tv, table(id, time_2))

tv_wide <- tv %>% pivot_wider(id_cols)
