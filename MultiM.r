# Multilevel Modeling workshop:
install.packages(c("multilevel", "lme4", "lmerTest", dep = T))

library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)

# Look at the variable names in 'education' dataset:
education %>% names()
# 'grpid': group ID, 'pid': student ID, 'hours': hours of study, 'partic': student participation, 'pcohesion', 'gender': student gender, 'tage': teacher age, 'tgender': teacher gender, 'perf': student performance
education %>% head()

# try linear regression
summary(lm(perf~gender + tage + tgender + hours + partic + pcohesion, data = education))

# Step 1:
gls.null <- gls(perf ~ 1, data = education)
lme.null <- lme(perf ~1, random = ~1|grpid, data = education)
summary(gls.null)
summary(lme.null)
# estimate of the intercept is same but SE and df for estimates = differ!
# SE for lme model is larger, ignoring random variation leads to overly precise results


anova(gls.null, lme.null)
#          Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# gls.null     1  2 7207.389 7217.203 -3601.695                        
# lme.null     2  3 5682.583 5697.303 -2838.291 1 vs 2 1526.806  <.0001

# model with varying intercept is significantly better than model with fixed intercept! (L.Ratio = 1526.806, p < 0.0001)



# ML for each classroom
ols.list <- lmList(perf ~ gender + hours + partic + pcohesion|grpid, data = education)
summary(ols.list)

# STEP 2: add predictors
# show in step 1 that with random intercept = imrpoves model! (shown by level-2 error)
# add level-2 predictors (another predictor to explain the variance of the intercept)
# Hypothesis: intercept variation fomr another level-2 predictor rather than just error
# only difference from LM: error for residual, error for intercept
# add level-1 and level-2 predictors by adding to "fixed" part of lme equation.
# test effect of level-1 and level-2 through t-test
# from t-test, calculate p-value given df.

# in education.df only have 2 group-level predictors (tage and tgender)
# we can aggregate (hours, partic, pcohesion) to classroom level to create level-2 predictors!










