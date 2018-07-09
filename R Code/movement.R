########Initial workspace setup#######
setwd("C:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
ptm = proc.time()
logistic_data = data.frame(read.csv(file="C:/Users/olive/Google Drive/Alex_data_and_SAS_files/movement_data.csv"))#calls loglin dataset 
options(scipen=999)

########## Calls in Packages####### 
library(car) #regession utilities
library(lme4)# linearmodels 
library(nlme)# non-linear and linear mixed effect models with ability to adjust covariance structure
library(lmerTest)# added utility to linear models 
library(emmeans)# adds SAS like lsmeans functionality
library(dplyr)# data manipulator 
library(e1071)# addition math functions and utility; allows for adjustments in kurtosis and skewness calc. to match SAS
library(multcompView)# tukey groups
library(MASS)
library(MuMIn)
library(optimx)

######### arrange the data############
colnames(logistic_data) = c("id","age","inside","outside","yearclass","year","origin","pool")# fixes column names
logistic_data$age = factor(logistic_data$age, levels = c("0", "1", "2"))# makes age a factor and forces ordering
logistic_data$year= factor(logistic_data$year, levels = c("2010", "2011", "2012", "2013", "2014", "2015"))# forces the ordering(levels) of year and makes it descrete rather than continuous
logistic_data$yearclass= factor(logistic_data$yearclass, levels = c("2010", "2011", "2012", "2013", "2014", "2015"))# forces the ordering(levels) of yearclass and makes it descrete rather than continuous
logistic_data = arrange(logistic_data,pool,origin,id,yearclass,year)# sorts data set by site,month,year

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 

##########Basic descriptive statistics of inside movement-my R equivalent to SAS proc univariate ######### 
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats_inside = summarise(group_by(logistic_data,pool),# applys following statistics by group 
                              Mean = mean(inside), 
                              N = length(inside),# number of observations
                              SD = sd(inside),# standard deviation 
                              SE = se(inside),# standard error 
                              Median = median(inside), 
                              Skewness = skewness(inside, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                              Kurtosis = kurtosis(inside, type = 2),# type 2 corrisponds to the same estimations method used by sas
                              Fifth_percentile= quantile(inside, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(inside, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 

Descriptive_stats_outside = summarise(group_by(logistic_data,pool),# applys following statistics by group 
                                     Mean = mean(outside), 
                                     N = length(outside),# number of observations
                                     SD = sd(outside),# standard deviation 
                                     SE = se(outside),# standard error 
                                     Median = median(outside), 
                                     Skewness = skewness(outside, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                                     Kurtosis = kurtosis(outside, type = 2),# type 2 corrisponds to the same estimations method used by sas
                                     Fifth_percentile= quantile(outside, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                                     Ninety_Fifth_percentile= quantile(outside, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 

remove(se, percentile)# cleans up work environment to point



#################Generalized linear mixed effects model (binomial distribution, Logit link) for outside movement##############
options(na.action = "na.fail")   #  prevent fitting models to different datasets
# Sauger_out_idr = glmer(outside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|id), data=logistic_data, family="binomial",control=glmerControl(optimizer= "bobyqa",optCtrl=list(maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_idr, trace=2)
# Sauger_out_nest =  glmer(outside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|yearclass/id), data=logistic_data, family="binomial",control=glmerControl(optimizer= "bobyqa",optCtrl=list(maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_nest, trace=2)
Sauger_full_outside = glmer(outside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|yearclass), data=logistic_data, family="binomial",control=glmerControl(optimizer= "bobyqa",optCtrl=list(maxfun=100000)))# logistic model 
dredge(Sauger_full_outside, trace=2)
outside_movement =  glmer(outside ~ age + origin + pool + age*pool +(1|yearclass), data=logistic_data, family="binomial",control=glmerControl(optimizer= "bobyqa",optCtrl=list(maxfun=100000)))
summary(outside_movement)
sum_temp = summary(outside_movement)
outside_sum = data.frame(sum_temp$coefficients)

temp_eemeans = emmeans(outside_movement,"age", by="pool")# creats marginal means object
tukey_pairwise_outside = broom::tidy(contrast(temp_eemeans, by= "pool",method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
cld_sum_outside = cld(temp_eemeans)
plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model, throws minor  error in ploting

#################generalized linear model (binomial distribution, Logit link) for inside movement ##############
# Sauger_out_idr = glmer(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|id), data=logistic_data, family="binomial",control=glmerControl(optimizer= "optimx",optCtrl=list(method= "L-BFGS-B", maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_idr, trace=2)
# Sauger_out_ycr = glmer(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|yearclass), data=logistic_data, family="binomial",control=glmerControl(optimizer= "optimx",optCtrl=list(method= "L-BFGS-B", maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_ycr, trace=2)
Sauger_full_inside =  glm(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin, data=logistic_data, family="binomial")# logistic model 
dredge(Sauger_full_inside, trace=2)
inside_movement = glm(inside ~ age + origin + pool + age*pool, data=logistic_data, family="binomial")
summary(inside_movement)
sum_temp = summary(inside_movement)
inside_sum = data.frame(sum_temp$coefficients)

temp_eemeans = emmeans(inside_movement,"age", by="pool")# creats marginal means object
tukey_pairwise_inside = broom::tidy(contrast(temp_eemeans, by= "pool",method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
cld_sum_inside = cld(temp_eemeans)
plot(temp_eemeans, comparisons =TRUE)

remove(Sauger_full_inside,Sauger_full_outside,sum_temp)

######Prediction Intervals for inside movement- response(use response scaled) and link scale#######
preds_outside = merTools::predictInterval(outside_movement, level = 0.95, n.sims = 1000, stat = "mean",type = "probability")

######Prediction Intervals for inside movement- response(use response scaled) and link scale#######
preds_link = predict( inside_movement, type = "link", se.fit = TRUE)# use the knowns regression to make predictions from  5th and 95th percentile water, also provides  std. error
critval <- 1.96 # critical value for approx 95% CI
upr <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction
lwr <- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction
fit <- preds_link$fit# returns fited value
lwr2 <-inside_movement$family$linkinv(upr)
upr2 <-inside_movement$family$linkinv(lwr)
fit2 <-inside_movement$family$linkinv(fit)
preds_link = data.frame(fit2,lwr2,upr2, logistic_data$id, logistic_data$age, logistic_data$yearclass, logistic_data$origin, logistic_data$pool)# puts predictions, CI, site 
colnames(preds_link) = c("Prediction","LCL", "UCL","ID","Age","Year_class","Origin","Pool")# give variables logical names
preds_inside = arrange(preds_link,Pool,Origin,Year_class,Age)# sorts data set by site,month,year

remove(fit,lwr,upr)# cleans up work environment
proc.time()-ptm
