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

# Get the -2LL and the difference of the 2 models.
logLik(gls.null); logLik(lme.null)
# 'log Lik.' -3601.695 (df=2)
# 'log Lik.' -2838.291 (df=3)

# of use anova():
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

# Hypothesis: intercept variation from another level-2 predictor rather than just error
# only difference from LM: error for residual, error for intercept

# add level-1 and level-2 predictors by adding to "fixed" part of lme equation.
# test effect of level-1 and level-2 through t-test
# from t-test, calculate p-value given df.

# evaluation of predictor significance through model comparisons with/without predictors (ANOVA as earlier)
# however, cannot compare models if fitted with Restricted-Max.Likelihood, necessity refit with Max. Likelihood

# Specify using ' method = "ML" ' in lme():
# lme(y ~ x1 + x2 + x3, random = ~1|Grouping_Variable, data = DATASET, method = "ML")

# Use drop1() to test ALL predictors iteratively instead of using anova() command:
# drop1(fullmodel, test = "Chisq")

# in education.df only have 2 group-level predictors (tage and tgender)
# we can aggregate (hours, partic, pcohesion) to classroom level to create level-2 predictors!
# after fit model, extract variance components using VarCorr()
# from examining the variance components of models with/without predictors = estimate pseudo-R^2

# Add level-1 predictor (ex. gender)
lme.m1 <- lme(perf ~ gender, 
              random = ~1|grpid, data = education)
summary(lme.m1)

VarCorr(lme.null)
#              Variance StdDev  
# (Intercept)  67.96178 8.243893
#  Residual    11.36346 3.370973
VarCorr(lme.m1)
#              Variance StdDev  
# (Intercept)  67.96969 8.244373
#  Residual    11.28013 3.358590

# R^squared
1 - 11.28/11.36     # 0.007    ~0.7% change, from only adding one variable (gender), significant. 

# add other predictors
lme.m2 <- lme(perf ~ gender + hours + partic + pcohesion, 
              random = ~1|grpid, data = education)
summary(lme.m2)

VarCorr(lme.m2)
#             Variance  StdDev  
# (Intercept) 45.646077 6.756188
#  Residual     1.978047 1.406430

# R^squared
1 - 1.97/ 11.36     # 0.82    ~82% model explained, from adding all other variables. (compared to null)

1 - 45.64/67.96
# expalined 33% between groups variance without including any group-level predictors... ??????
# R-squared not good explanation for estimate fit of Multi-level models
# use max.likelihood (nested models) for fit based on null model.


# Refit model with Max.Likelihood
lme.m2ml <- lme(perf~gender + hours + partic + pcohesion, random = ~1|grpid, 
                method = "ML", data = education)
drop1(lme.m2ml, test = "Chisq")

lme.m2ml2 <- lme(perf ~ hours + partic + pcohesion, random = ~1|grpid, 
                method = "ML", data = education)
anova(lme.m2ml, lme.m2ml2)
#           Model df      AIC      BIC    logLik     Test    L.Ratio    p-value
# lme.m2ml      1  7   4073.544 4107.898 -2029.772                             
# lme.m2ml2     2  6   4072.418 4101.864 -2030.209   1 vs 2  0.8737226   0.3499

# NO significant difference found (p = 0.3499), gender variable NOT useful for explaining perf.
# Can also use drop1():
drop1(lme.m2ml, test = "Chisq")


temp1 <- aggregate(cbind(education$hours, education$partic, education$pcohesion), 
                  by = list(education$grpid), mean)
names(temp1) <- c("grpid", "hours.grp", "partic.grp", "pcohesion.grp")
temp1

education <- merge(education, temp1, by = "grpid", all = T)

head(education)

lme.m3 <- lme(perf ~ gender + tage + tgender + hours + partic + pcohesion + 
                hours.grp + partic.grp + pcohesion.grp, 
              random = ~1|grpid, data = education)

# mucked up the merge so take out all the redundant columns and try again............
edu <- education[1:9,]

education <- merge(edu, temp1, by = "grpid", all = T)    # all = TRUE, keep data and assign NA values
head(education)

lme.m3 <- lme(perf ~ gender + tage + tgender + hours + partic + pcohesion + 
                hours.grp + partic.grp + pcohesion.grp, 
              random = ~1|grpid, data = education)
summary(lme.m3)
# Fixed effects:
# Significant: hours, partic, pcohesion, hours.grp, pcohesion.grp
# study hours of student have positive effect but avg. study hours of the classroom had negative effect (?)
# ^ made-up dataset so LOL.
VarCorr(lme.m3)
# evaluate signifiance with conf.int, for lme use intervals(), assumption: t-distributions with df as in model
intervals(lme.m3)

# STEP 3: random slopes
# test hypothesis: effect of level-1 predictor = DIFFERENT in each group
# additional error term for different slopes per group

# Check for slope variability between level-1 variable per different groups
ggplot(education[1:160, ], aes(x = hours, y = perf, color = factor(grpid))) +
  geom_point() + stat_smooth(method = "lm", fullrange = TRUE) +
  facet_wrap(~grpid)

# fit model and compare with fixed slope model
lme.m4 <- lme(perf ~ gender + tage + tgender + hours + partic + pcohesion + 
                hours.grp + partic.grp + pcohesion.grp, 
              random = ~hours|grpid, data = education)
summary(lme.m4)
anova(lme.m3, lme.m4)

# for groups, higher performing , the effect of hours was smaller: -0.952 corr

# ONLY varying slope
lme.m4a <- lme(perf ~ gender + tage + tgender + hours + partic + pcohesion + 
                hours.grp + partic.grp + pcohesion.grp, 
              random = ~ -hours|grpid, data = education)
anova(lme.m4, lme.m4a)

# extract random effect
ranef(lme.m4)
plot(ranef(lme.m4))
plot(ranef(lme.m4)[order(ranef(lme.m4)[[1]]),])
# 

lme.m4ml <- lme(perf ~ gender + tage + tgender + hours + partic + pcohesion + 
                 hours.grp + partic.grp + pcohesion.grp, 
               random = ~ -hours|grpid, method = "ML", data = education)
drop1(lme.m4ml, test = "Chisq")



