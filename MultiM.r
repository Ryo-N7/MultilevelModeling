# Multilevel Modeling workshop:
install.packages(c("multilevel", "lme4", "lmerTest", dep = T))

library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)
library(multilevel)
library(splines)

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


# STEP 4: cross-level interactions
# use level-2 (group-level) predictors to explain variability of slopes
# substitute level-2 equations into level-1 equations
# level-2 predictor is multiplied (interacting) with level-1 predictor (of slope can vary between groups)
# when use level-2 predictors to model slove variability @ level-1 == test the INTERACTION between variables at the two different levels!

lme.m5 <- lme(perf ~ gender+tage+tgender+hours+partic+pcohesion+
                hours.grp + partic.grp + pcohesion.grp+
                hours*hours.grp + hours * partic.grp + hours * pcohesion.grp,
              random = ~hours|grpid, data = education)
summary(lme.m5)
VarCorr(lme.m5)
VarCorr(lme.m4)
# significant effect of hours of study at indiv. and perceivedcohesion group level on performance
# ^ cohesion in classroom, hours of study at individual level effect on performance becomes stronger!
1 - 0.809/0.9359     # ~13.5% variance explained by adding interaction effects of random slopes



# With Max.Likelihood to test if interactions = improve model fit?
lme.m5ml <- lme(perf ~ gender+tage+tgender+hours+partic+pcohesion+
                hours.grp + partic.grp + pcohesion.grp+
                hours*hours.grp + hours * partic.grp + hours * pcohesion.grp,
              random = ~hours|grpid, method = "ML", data = education)
drop1(lme.m5ml, test = "Chisq")






# Longitudinal data:
# observations over time = nested within higher level units such as individual/organizations/etc.
# time as random effect
# problem: regression toward mean over time, 
# therefore, necessity model for different trajectories (need ^^ data, splines, polynomial models...)
# advantage: missing responses from one time period DONT need to be excluded
# Order of observations = specific + important in model.
# plug time variable into model, test for fixed effect, test for variation of fixed effect between subject (the higher-level units...)
# slope of time = trajectory of change of DV

# add parameter: correlation = corAR1()   autoregressive of order 1 (previous iteration will influence next iteration)
# test if improve model by comparison to model without AR correlation matrix
# hypotheses: cross-level interaction between time and predictor measured at higher level
# time = varying effect, cross-level means higher-level predictor explains the varying slopes for TIME

# Step 1: Examine if varying intercept imrpoves model
# Step 2a: add TIME as fixed effect
# Step 2b: specify TIME as random effect and comparison with Model 2a
# Step 2c: test for serial correlation and comparison with Model 2b
# Step 3: add level-1 and level-2 predictors
# Step 4:Examine if varying slope for level-1 predictors improves model
# Step 5:Add level-2 predictors to explain slope variation (cross-level interactions)

rm(list = ls())
names(migraine)
head(migraine)
# pid = person id, week = # week, age = age, treatment = # of treatment given per week, migr = migraine

migraine <- migraine[order(as.numeric(migraine$pid)),]
ggplot(migraine[1:832, ], aes(x = week, y = migr, color = pid)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ ns(x, 3)) + 
  facet_wrap(~pid)

# Step 1
gls.null <- gls(migr~1, data = migraine)
lme.null <- lme(migr ~ 1, random = ~1|pid, data = migraine)
anova(gls.null, lme.null)
# significant difference!

# Step 2
# TIME variable start from ZERO
# Transform week variable so Week 1 = 0 in dataset
migraine$week<-migraine$week-1
lme.m1<-lme(migr~week, random=~1|pid, data=migraine)
summary(lme.m1)
# negative association

# polynomial models ex. quadratic model, poly() >>> difficult to interpret results
lme.m1b<-lme(migr~week+I(week^2), random=~1|pid, data=migraine)
summary(lme.m1b)

lme.m1c<-lme(migr~ns(week,3), random=~1|pid, data=migraine)
summary(lme.m1c)

# Random effect for week
lme.m2<-lme(migr~week, random=~week|pid, data=migraine)
anova(lme.m1, lme.m2)
summary(lme.m2)
# significant difference between model with fixed slope for effect weeks and random slope for effect weeks (time)

# Add auto-correlation matrix
lme.m3<-lme(migr~week, random=~week|pid, correlation=corAR1(), data=migraine)
anova(lme.m2, lme.m3)
# NO significant difference, no improvement in model fit from adding auto-correlation

# Step 3: add predictors to model
lme.m4<-lme(migr~age+week+treat, random=~week|pid, data=migraine)
lme.m4ml<-lme(migr~age+week+treat, random=~week|pid, 
            method = "ML", data=migraine)
drop1(lme.m4ml, test = "Chisq")
summary(lme.m4)

# Skip Step 4 as no need to add more random effects

# Step 5: cross-level interaction between week of treatments and # of treatments
# explain steepness of trajectory of migrains (rate of change in migraines)
# predict how fast patient is improving (bigger slope = faster improvement)
lme.m5<-lme(migr~age+week*treat, random=~week|pid, data=migraine)
summary(lme.m5)
1 - 0.0001820253/0.0002234984   # 18% variation of migraine intensity explained by interaction of week and# of treatments

# week:treat coefficient: -0.005821
# # of treatments/week is negatively associated with the rate of change (slope of migraines became weaker at faster pace) in the experience of migraine intensity
# more treatments, migraine intensity decrease at faster pace
# # of treatments/week NOT direct effect on migraine intensity
# for every additional treatment, impact on slope of migraine intensity over TIME
# person with 3 treatment/week over 20 weeks have more migraine intensity compared to
# person with 4 treatment/week over 20 weeks as negative slope of migraine intensity is stronger

# week coefficient: 0.000634
ranef(lme.m5)    # week   summed = ~0


# all regression assumptions apply = necessity check beforehand, VIF/multicollinearity etc etc. etc.





