
#reading in longitudinal data
ecls.l <- read_sas("datasets/eclsk_longitudinal.sas7bdat")
vtable(ecls.l)

#need numeric timepoint
str(ecls.l$timepoint)
#was read in as character variable

#if no var exists like ecls$time, which is numeric...
ecls.l$num.time <- factor(ecls.l$timepoint,
                          levels = c("KS", "FirstS", "ThirdS"))
ecls.l$num.time <- as.numeric(ecls.l$num.time) - 1
#writes 0, 1, 2 for time points


#baseline
m.x <- lmer(MTH_I ~ time + (time||CHILDID), data = ecls.l, na.action = na.omit)
summary(m1)
#"boundary (singular) fit" error, adding | to fix covariance of random effects to zero,
# error persisted, remove time

m.x <- lmer(MTH_I ~ time + (1|CHILDID), data = ecls.l, na.action = na.omit)
summary(m2)

#reading outcome baseline
m.x <- lmer(RD_I ~ time + (time||CHILDID), data = ecls.l, na.action = na.omit)

#
m.x <- lmer(MTH_I ~ time + white + hispanic + asian + multiracial +
             (1|CHILDID),
           data = ecls.l, na.action = na.omit)
summary(m4)

#can construct test statistic to determine whether there is a general effect of race
m1 <- lmer(MTH_I ~ time +
             (1|CHILDID),
           data = ecls.l, na.action = na.omit, REML = F)

m2 <- lmer(MTH_I ~ time + white + hispanic + asian + multiracial +
             (1|CHILDID),
           data = ecls.l, na.action = na.omit, REML = F)

#missing data in m2 that is in m1, some people didn't include race
#filter
ecls.l_f <- ecls.l %>% filter(!is.na(time), !is.na(white), !is.na(hispanic), !is.na(asian), !is.na(multiracial))


anova(m1, m2)
