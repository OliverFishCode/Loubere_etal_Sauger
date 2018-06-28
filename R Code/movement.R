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
outside_sum = data.frame(outside_movement)
temp_eemeans = emmeans(outside_movement,"age", by="pool")# creats marginal means object
tukey_pairwise_outside = broom::tidy(contrast(temp_eemeans, by= "pool",method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
cld_sum = cld(temp_eemeans)
plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model, throws minor  error in ploting

#################generalized linear model (binomial distribution, Logit link) for inside movement ##############
# Sauger_out_idr = glmer(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|id), data=logistic_data, family="binomial",control=glmerControl(optimizer= "optimx",optCtrl=list(method= "L-BFGS-B", maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_idr, trace=2)
# Sauger_out_ycr = glmer(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin +(1|yearclass), data=logistic_data, family="binomial",control=glmerControl(optimizer= "optimx",optCtrl=list(method= "L-BFGS-B", maxfun=100000)))# logistic model to test differences in value by site
# dredge(Sauger_out_ycr, trace=2)
Sauger_full_inside =  glm(inside ~ pool + origin + age+ age*pool+ pool*origin +age*origin, data=logistic_data, family="binomial")# logistic model 
dredge(Sauger_full_inside, trace=2)
inside_movement = glm(inside ~ age + origin + pool + age*origin + age*pool, data=logistic_data, family="binomial")
summary(inside_movement)
sum_temp = summary(inside_movement)
inside_sum = data.frame(sum_temp$coefficients)

remove(Sauger_full_inside,Sauger_full_outside,inside_movement,outside_movement,sum_temp)
proc.time()-ptm



# #########compare multi-comp outcomes#######
# plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model
# plot(temp_eemeans2, comparisons =TRUE)# Pairwise comaparisons plot for the temporal autocorrelation linear model
# cld(temp_eemeans)# compact letter display (tukey groupings) for basic linear model
# cld(temp_eemeans2)# compact letter display (tukey groupings) for temporal autocorrelation linear model
# remove(temp_eemeans2,temp_eemeans,temporal_CL_lm,Sr_temporal_lm,temp_sum)# cleans up work environment to point




# #####Prediction Intervals for otolith to water- response(use response scaled) and link scale#######
# oto_inverse =  data.frame(x$linkfun(SR_reg_data$otolith))
# colnames(oto_inverse) = c("inverse_oto")# give variables logical names
# SR_reg_data = data.frame(SR_reg_data, oto_inverse)
# preds_link = predict.glm( Sr_oto_water, newdata = SR_reg_data, type = "link", se.fit = TRUE)# use the knowns regression to make predictions from  5th and 95th percentile water, also provides  std. error
# critval <- 1.96 # critical value for approx 95% CI
# upr <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction
# lwr <- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction
# fit <- preds_link$fit# returns fited value
# lwr2 <-Sr_oto_water$family$linkinv(upr)
# upr2 <-Sr_oto_water$family$linkinv(lwr)
# fit2 <-Sr_oto_water$family$linkinv(fit)
# preds_link = data.frame(fit2,lwr2,upr2,fit,lwr,upr,SR_reg_data$site, SR_reg_data$water, SR_reg_data$otolith)# puts predictions, CI, site ,and measured otolith and water values in a single dataframe
# colnames(preds_link) = c("Prediction","LCL", "UCL","Link_Prediction","Link_LCL", "Link_UCL", "Site","Water","Otolith")# give variables logical names
# preds_link = arrange(preds_link, Prediction,Water)# sorts data set by site,month,year
# plot(otolith~water, SR_reg_data)# plots measured water and otolith values on link scale
# lines(preds_link$Water,preds_link$Prediction, col="red",lty=2,lwd=3)# adds fitted line
# lines(preds_link$Water,preds_link$LCL,col="blue",lty=2,lwd=3)# adds LCL line
# lines(preds_link$Water,preds_link$UCL,  col="blue",lty=2,lwd=3)# adds UCL line
# plot(inverse_oto~water, SR_reg_data)# plots measured water and otolith values on link scale
# lines(preds_link$Water,preds_link$Link_Prediction, col="red",lty=2,lwd=3)# adds fitted line
# lines(preds_link$Water,preds_link$Link_LCL,col="blue",lty=2,lwd=3)# adds LCL line
# lines(preds_link$Water,preds_link$Link_UCL,  col="blue",lty=2,lwd=3)# adds UCL line
# remove(x, critval,fit,lwr,r2,upr)# cleans up work environment
# 
