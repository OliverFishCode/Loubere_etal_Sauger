########Initial workspace setup#######
setwd("C:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
logistic_data = data.frame(read.csv(file="C:/Users/olive/Google Drive/Alex_data_and_SAS_files/movement_data.csv"))#calls loglin dataset 
#SR_data$value = log(SR_data$value)# natural log of response if needed
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
library(DHARMa)#simulated qq check

######### arrange the data############
colnames(logistic_data) = c("id","age","inside","outside","yearclass","year","origin","pool")# fixes column names
logistic_data$age = factor(logistic_data$age, levels = c("0", "1", "2", "3", "4"))# makes age a factor and forces ordering
logistic_data$year= factor(logistic_data$year, levels = c("2010", "2011", "2012", "2013", "2014", "2015"))# forces the ordering(levels) of year and makes it descrete rather than continuous
logistic_data$yearclass= factor(logistic_data$yearclass, levels = c("2010", "2011", "2012", "2013", "2014", "2015"))# forces the ordering(levels) of yearclass and makes it descrete rather than continuous
logistic_data = arrange(logistic_data,pool,origin,id,yearclass,year)# sorts data set by site,month,year

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 

##########Basic descriptive statistics of inside movement-my R equivalent to SAS proc univariate ######### 
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats_inside = summarise(group_by(logistic_data,age),# applys following statistics by group 
                              Mean = mean(inside), 
                              N = length(inside),# number of observations
                              SD = sd(inside),# standard deviation 
                              SE = se(inside),# standard error 
                              Median = median(inside), 
                              Skewness = skewness(inside, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                              Kurtosis = kurtosis(inside, type = 2),# type 2 corrisponds to the same estimations method used by sas
                              Fifth_percentile= quantile(inside, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(inside, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 

Descriptive_stats_outside = summarise(group_by(logistic_data,age),# applys following statistics by group 
                                     Mean = mean(outside), 
                                     N = length(outside),# number of observations
                                     SD = sd(outside),# standard deviation 
                                     SE = se(outside),# standard error 
                                     Median = median(outside), 
                                     Skewness = skewness(outside, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                                     Kurtosis = kurtosis(outside, type = 2),# type 2 corrisponds to the same estimations method used by sas
                                     Fifth_percentile= quantile(outside, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                                     Ninety_Fifth_percentile= quantile(outside, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 

levenetest_origin = leveneTest(logistic_data$outside, logistic_data$origin) #levene homogeniety of variance test r; requires atleast 2 values per group 
levenetest_pool = leveneTest(loglin_data$count,loglin_data$pool) #levene test is used instead of bartletts because its robust to non-normal data
leventest_age = leveneTest()



remove(se, percentile)# cleans up work environment to point



#################Water Linear Model- includes f-test for overall model##############
Sr_site_lm = glm(value ~ site, SR_data, family = "gaussian")# linear model to test differences in value by site
summary(Sr_site_lm)# prints summary of linear coefficients and significance 
lm_summary =broom::tidy(Sr_site_lm)# summary in a pretty dataframe
glm_sum = summary(Sr_site_lm)# creates summary variable
resid_df = as.numeric(glm_sum$df.residual)# extracts and creates residual df variable
null_df = as.numeric(glm_sum$df.null)# extracts and creates model df variable
msr = (glm_sum$null.deviance - glm_sum$deviance)/(null_df - resid_df)# calculates mean square regression
mse = glm_sum$deviance/resid_df# calculates mean square error
Model_F_stat = msr/mse# calculates F value
P_or_F = data.frame(2 * pf(q=Model_F_stat, df1=(null_df - resid_df), df2=(resid_df), lower.tail=FALSE))# 2-tail probability of F
Model_F_test_lm = data.frame(Model_F_stat,P_or_F)# puts F test in dataframe
CL_lm = broom::confint_tidy(Sr_site_lm, conf.level=0.95)# 95% clm on linear coefficients
temp_eemeans = emmeans(Sr_site_lm,"site")# creats marginal means object
tukey_pairwise_lm = broom::tidy(contrast(temp_eemeans, method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
lm_summary = data.frame(lm_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,CL_lm, Sr_site_lm)# cleans up work environment to point

#######Water Linear Mixed Model with AR(1) R-side error correction########
Sr_temporal_lm = gls(value ~ site,  correlation = corAR1(form = ~1|year/month) , data = SR_data)# linear model to test differences in value by site
temp_sum = summary(Sr_temporal_lm)# prints summary of linear coefficients and significance 
temporal_summary = data.frame(temp_sum$tTable)
temporal_CL_lm = broom::confint_tidy(Sr_temporal_lm, conf.level=0.95)# 95% clm on linear coefficients
temp_eemeans2 = emmeans(Sr_temporal_lm,"site")# creats marginal means object
tukey_pairwise_temporal_site = broom::tidy(contrast(temp_eemeans2, method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe for site
temporal_summary = data.frame(temporal_summary,temporal_CL_lm)# combines linear coeffecient summary and CLM into same dataframe

#########compare multi-comp outcomes#######
plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model
plot(temp_eemeans2, comparisons =TRUE)# Pairwise comaparisons plot for the temporal autocorrelation linear model
cld(temp_eemeans)# compact letter display (tukey groupings) for basic linear model
cld(temp_eemeans2)# compact letter display (tukey groupings) for temporal autocorrelation linear model
remove(temp_eemeans2,temp_eemeans,temporal_CL_lm,Sr_temporal_lm,temp_sum)# cleans up work environment to point


#################Knowns (water otolith relationship) Linear Model- includes f-test for overall model##############
Sr_oto_water = glm(otolith ~ water, data = SR_reg_data, family = "Gamma")# linear model to test differences in value by site
summary(Sr_oto_water)# prints summary of linear coefficients and significance 
reg_summary =broom::tidy(Sr_oto_water)# summary in a pretty dataframe
glm_sum = summary(Sr_oto_water)# creates summary variable
resid_df = as.numeric(glm_sum$df.residual)# extracts and creates residual df variable
null_df = as.numeric(glm_sum$df.null)# extracts and creates model df variable
msr = (glm_sum$null.deviance - glm_sum$deviance)/(null_df - resid_df)# calculates mean square regression
mse = glm_sum$deviance/resid_df# calculates mean square error
Model_F_stat = msr/mse# calculates F value
P_or_F = data.frame(2 * pf(q=Model_F_stat, df1=(null_df - resid_df), df2=(resid_df), lower.tail=FALSE))# 2-tail probability of F
r2 = 1- (Sr_oto_water$deviance/Sr_oto_water$null.deviance)
known_reg_fandr2 = data.frame(Model_F_stat,P_or_F,r2)# puts F test in dataframe
colnames(known_reg_fandr2) = c("F value","P", "R2")# give variables logical names
CL_lm = broom::confint_tidy(Sr_oto_water, conf.level=0.95)# 95% clm on linear coefficients
water_reg_summary = data.frame(reg_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,CL_lm,reg_summary)# cleans up work environment to point


#####Prediction Intervals for otolith to water- response(use response scaled) and link scale#######
oto_inverse =  data.frame(x$linkfun(SR_reg_data$otolith))
colnames(oto_inverse) = c("inverse_oto")# give variables logical names
SR_reg_data = data.frame(SR_reg_data, oto_inverse)
preds_link = predict.glm( Sr_oto_water, newdata = SR_reg_data, type = "link", se.fit = TRUE)# use the knowns regression to make predictions from  5th and 95th percentile water, also provides  std. error
critval <- 1.96 # critical value for approx 95% CI
upr <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction
lwr <- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction
fit <- preds_link$fit# returns fited value
lwr2 <-Sr_oto_water$family$linkinv(upr)
upr2 <-Sr_oto_water$family$linkinv(lwr)
fit2 <-Sr_oto_water$family$linkinv(fit)
preds_link = data.frame(fit2,lwr2,upr2,fit,lwr,upr,SR_reg_data$site, SR_reg_data$water, SR_reg_data$otolith)# puts predictions, CI, site ,and measured otolith and water values in a single dataframe
colnames(preds_link) = c("Prediction","LCL", "UCL","Link_Prediction","Link_LCL", "Link_UCL", "Site","Water","Otolith")# give variables logical names
preds_link = arrange(preds_link, Prediction,Water)# sorts data set by site,month,year
plot(otolith~water, SR_reg_data)# plots measured water and otolith values on link scale
lines(preds_link$Water,preds_link$Prediction, col="red",lty=2,lwd=3)# adds fitted line
lines(preds_link$Water,preds_link$LCL,col="blue",lty=2,lwd=3)# adds LCL line
lines(preds_link$Water,preds_link$UCL,  col="blue",lty=2,lwd=3)# adds UCL line
plot(inverse_oto~water, SR_reg_data)# plots measured water and otolith values on link scale
lines(preds_link$Water,preds_link$Link_Prediction, col="red",lty=2,lwd=3)# adds fitted line
lines(preds_link$Water,preds_link$Link_LCL,col="blue",lty=2,lwd=3)# adds LCL line
lines(preds_link$Water,preds_link$Link_UCL,  col="blue",lty=2,lwd=3)# adds UCL line
remove(x, critval,fit,lwr,r2,upr)# cleans up work environment

#################Wilcoxon model  code included to provide non-parametric example ############## 
#kruskal.test(value~site, SR_data)
#pairwise.wilcox.test(SR_data$value, SR_data$site,p.adjust.method = "bonferroni" )