library(haven)
library(here)
library(vtable)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(nlme)


ecls <- read_sas("datasets/eclsk_thirds_combined.sas7bdat")
vtable(ecls)

table(ecls$teacherid)

#m1 <- lm(RD_T ~ SES, filter(ecls, teacherid=="0007T41"))
#m1


#m2 <- lm(RD_T ~ SES, filter(ecls, teacherid=="0052T41"))
#m3 <- lm(RD_T ~ SES, filter(ecls, teacherid=="0067T42"))
#m4 <- lm(RD_T ~ SES, filter(ecls, teacherid=="0085T41"))

#m2
#m3
#m4

#example
ggplot(data = ecls, aes(x = SES, y = RD_T)) +
  geom_point(alpha = .3, size = .5) +
  geom_smooth(method = "lm") +
  theme_bw()

#all reg lines
ggplot(data = ecls, aes(x = SES, y = RD_T, group = factor(teacherid))) +
  geom_line(stat = "smooth", method = "lm", alpha = .2, fullrange = T) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw()
  
#overall reg line
ggplot(data = ecls, aes(x = SES, y = RD_T, group = factor(teacherid)), stat = "smooth",
       method = "lm", alpha = .3, fullrange = T)
    

#fitting baseline model
ecls.nomiss <- filter(ecls, !is.na(RD_T))

ecls.nomiss$teacherid <- as.factor(ecls.nomiss$teacherid)
m2 <- lmer(RD_T ~ 1 + (1|teacherid), data = ecls.nomiss)
#in formula, intercept represented as 1
#parentheses with pipe represents random effects
#


#ICC calc
14.14/66.65

#root design effect calc
sqrt(1+.21*(9.84-1))

#what if we ignored clustering?
lm(RD_T ~ 1, data = ecls.nomiss)

#--------------------------------------------------------------------

#oneway ANCOVA with random effects
m3 <- lmer(RD_T ~ SES + (1|teacherid), data = ecls, na.action = na.omit)

#means-as-outcomes model
m4 <- lmer(RD_T ~ SES_Mean + (1|teacherid), data = ecls, na.action = na.omit)

#random coefficient model
m5 <- lmer(RD_T ~ SES + (1 + SES|teacherid), data = ecls, na.action = na.omit)
