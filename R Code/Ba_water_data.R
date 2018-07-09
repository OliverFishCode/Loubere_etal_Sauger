########Initial workspace setup#######
setwd("C:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
Ba_data = data.frame(read.csv(file="C:/Users/olive/Google Drive/Alex_data_and_SAS_files/Ba_water_data.csv"))#calls SR_water dataset
known_data = data.frame(read.csv(file="C:/Users/olive/Google Drive/Alex_data_and_SAS_files/ba_knowns.csv"))#calls Knowns dataset 
known_data = droplevels(known_data[-which(known_data$site == "KIN"),])# removes SD to test effects on normality and homogeniety
Ba_data = droplevels(Ba_data[-which(Ba_data$site == "sal"),])# removes SD to test effects on normality and homogeniety
#known_data = droplevels(known_data[-which(known_data$site == "kank"),])# removes kank to test effects on normality and homogeniety
ar1 = Ba_data
ar1$value = log(ar1$value)# natural log of response if needed
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


######### arrange the data############
Ba_data$month = factor(Ba_data$month, levels = c("may", "june", "july", "aug", "sept"))# forces the ordering(levels) of month to be logical rather than alphabetical
Ba_data$year= factor(Ba_data$year, levels = c("2014", "2015", "2016", "2017"))# forces the ordering(levels) of year and makes it descrete rather than continuous
Ba_data = arrange(Ba_data,site,year,month)# sorts data set by site,month,year
colnames(known_data) = c("water","otolith","site")# fixes column names

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 
x = inverse.gaussian(link="inverse")# easy to use inverse link not the same as inverse of link

##########Basic descriptive statistics-my R equivalent to SAS proc univariate ######### #Kurtosis had to be removed do to error
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats = summarise(group_by(Ba_data,site),# applys following statistics by group 
                              Mean = mean(value), 
                              N = length(value),# number of observations
                              SD = sd(value),# standard deviation 
                              SE = se(value),# standard error 
                              Median = median(value), 
                              Skewness = skewness(value, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                              Fifth_percentile= quantile(value, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(value, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 
remove(se, percentile)# cleans up work environment to point

########Add 5th and 95th percentile values to data for predictions##########
otolith = ""# filler for ottolith values i.e., creates variable column so that the new data set has same dimensions as known_data and they can be appended
water = Descriptive_stats$Fifth_percentile# creates variable with levels containing 5th percentile water values by site
site = Descriptive_stats$site# adds site variable
Site_low = data.frame(water, otolith, site)# creates new data set site_low
Site_low$site = plyr::revalue(Site_low$site,c("cumb"="lcumb", "mmr"="lmmr", "ohr"="lohr", "tenn"="ltenn", "twr"="ltwr", "union"="lunion", "utrib"="lutrib", "wab"="lwab", "emb"="lemb", "white"="lwhite"))# applys unique variable names to 5th percentile values

water = Descriptive_stats$Ninety_Fifth_percentile# creates variable with levels containing 95th percentile water values by site
Site_high = data.frame(water, otolith, site)# creates new data set site_high
Site_high$site = plyr::revalue(Site_high$site,c("cumb"="ucumb", "mmr"="ummr", "ohr"="uohr", "tenn"="utenn", "twr"="utwr", "union"="uunion", "utrib"="uutrib", "wab"="uwab", "emb"="uemb", "white"="uwhite"))# applys unique variable names to 95th percentile values
Ba_reg_data = rbind(known_data, Site_high, Site_low)# combines site_low and site_high into knowns data set
Ba_reg_data$otolith = as.numeric(Ba_reg_data$otolith)# changes values from character to numeric
remove(Site_high,Site_low, water, site, otolith, known_data)# cleans up work environment to point

##########Test assumptions of normality and homoscedasticity- mix of proc univariate and glm bartlett test#########
temp_norm = shapiro.test(Ba_data$value)# normality test 
P = data.frame(temp_norm$p.value)# temporary variable to contain P
W = data.frame(temp_norm$statistic)# temporary variable to contain w statistic
Shapiro_wilks_water = data.frame(W,P)# puts normality test output into dataframe
colnames(Shapiro_wilks_water) = c("W","P")# give variables logical names

temp_norm = shapiro.test(Ba_reg_data$otolith)# normality test 
P = data.frame(temp_norm$p.value)# temporary variable to contain P
W = data.frame(temp_norm$statistic)# temporary variable to contain w statistic
Shapiro_wilks_knowns = data.frame(W,P)# puts normality test output into dataframe
colnames(Shapiro_wilks_knowns) = c("W","P")# give variables logical names

temp_var = bartlett.test(value ~ site, data = Ba_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)# temporary variable to contain P
K = data.frame(temp_var$statistic)# temporary variable to contain k statistic
Bartlett_Homogen_water = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_water) = c("K","P")# give variables logical names

temp_var = bartlett.test(otolith ~ water, data = Ba_reg_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)# temporary variable to contain P
K = data.frame(temp_var$statistic)# temporary variable to contain k statistic
Bartlett_Homogen_knowns = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_knowns) = c("K","P")# give variables logical names
remove(P,W,temp_norm,temp_var,K,P_B)# cleans up work environment to point

# #################Water Linear Model- includes f-test for overall model##############
# Ba_site_lm = glm(value ~ site, Ba_data, family = gaussian(link = "log"))  # linear model to test differences in value by site
# summary(Ba_site_lm)# prints summary of linear coefficients and significance 
# lm_summary =broom::tidy(Ba_site_lm)# summary in a pretty dataframe
# glm_sum = summary(Ba_site_lm)# creates summary variable
# resid_df = as.numeric(glm_sum$df.residual)# extracts and creates residual df variable
# null_df = as.numeric(glm_sum$df.null)# extracts and creates model df variable
# msr = (glm_sum$null.deviance - glm_sum$deviance)/(null_df - resid_df)# calculates mean square regression
# mse = glm_sum$deviance/resid_df# calculates mean square error
# Model_F_stat = msr/mse# calculates F value
# P_or_F = data.frame(2 * pf(q=Model_F_stat, df1=(null_df - resid_df), df2=(resid_df), lower.tail=FALSE))# 2-tail probability of F
# Model_F_test_lm = data.frame(Model_F_stat,P_or_F)# puts F test in dataframe
# CL_lm = broom::confint_tidy(Ba_site_lm, conf.level=0.95)# 95% clm on linear coefficients
# temp_eemeans = emmeans(Ba_site_lm,"site")# creats marginal means object
# tukey_pairwise_lm = broom::tidy(contrast(temp_eemeans, method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
# lm_summary = data.frame(lm_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
# remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,CL_lm, Ba_site_lm)# cleans up work environment to point

#######Generalized least squares fit linear  model with AR(1)R-side error correction by year and month nested within year########
Ba_temporal_lm = gls(value ~ site, correlation = corAR1(form = ~1|year/month) , data = ar1)# linear model to test differences in value by site
temp_sum = summary(Ba_temporal_lm)# prints summary of linear coefficients and significance 
temporal_summary = data.frame(temp_sum$tTable)
temporal_CL_lm = broom::confint_tidy(Ba_temporal_lm, conf.level=0.95)# 95% clm on linear coefficients
temp_eemeans2 = emmeans(Ba_temporal_lm,"site")# creats marginal means object
tukey_pairwise_temporal_site = broom::tidy(contrast(temp_eemeans2, method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe for site
temporal_summary = data.frame(temporal_summary,temporal_CL_lm)# combines linear coeffecient summary and CLM into same dataframe

#########compare multi-comp outcomes#######
# plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model
plot(temp_eemeans2, comparisons =TRUE)# Pairwise comaparisons plot for the temporal autocorrelation linear model
# cld(temp_eemeans)# compact letter display (tukey groupings) for basic linear model
cld(temp_eemeans2)# compact letter display (tukey groupings) for temporal autocorrelation linear model
remove(temp_eemeans2,temporal_CL_lm,Ba_temporal_lm,temp_sum)# cleans up work environment to point


#################General linear model distribution = gaussian link = log, Knowns (water otolith relationship) Linear Model- includes f-test for overall model##############
Ba_oto_water = glm(otolith ~ water, data = Ba_reg_data, family = gaussian(link = "log"))# linear model to test differences in value by site
summary(Ba_oto_water)# prints summary of linear coefficients and significance 
reg_summary =broom::tidy(Ba_oto_water)# summary in a pretty dataframe
glm_sum = summary(Ba_oto_water)# creates summary variable
resid_df = as.numeric(glm_sum$df.residual)# extracts and creates residual df variable
null_df = as.numeric(glm_sum$df.null)# extracts and creates model df variable
msr = (glm_sum$null.deviance - glm_sum$deviance)/(null_df - resid_df)# calculates mean square regression
mse = glm_sum$deviance/resid_df# calculates mean square error
Model_F_stat = msr/mse# calculates F value
P_or_F = data.frame(2 * pf(q=Model_F_stat, df1=(null_df - resid_df), df2=(resid_df), lower.tail=FALSE))# 2-tail probability of F
r2 = 1- (Ba_oto_water$deviance/Ba_oto_water$null.deviance)
known_reg_fandr2 = data.frame(Model_F_stat,P_or_F,r2)# puts F test in dataframe
colnames(known_reg_fandr2) = c("F value","P", "R2")# give variables logical names
CL_lm = broom::confint_tidy(Ba_oto_water, conf.level=0.95)# 95% clm on linear coefficients
water_reg_summary = data.frame(reg_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,CL_lm,reg_summary)# cleans up work environment to point

#####Prediction Intervals for otolith to water- response scale (this is what you care about)#######
preds = predict.glm( Ba_oto_water, newdata = Ba_reg_data, type = "response", se.fit = TRUE)# use the knowns regression to make predictions from  5th and 95th percentile water, also provides  std. error
critval <- 1.96 # critical value for approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)# estimate upper CI for prediction
lwr <- preds$fit - (critval * preds$se.fit)# estimate lower CI for prediction
fit <- preds$fit# returns fited value
preds = data.frame(fit,lwr,upr,Ba_reg_data$site, Ba_reg_data$water, Ba_reg_data$otolith)# puts predictions, CI, site ,and measured otolith and water values in a single dataframe
colnames(preds) = c("Prediction","LCL", "UCL", "Site","Water","Otolith")# give variables logical names
preds = arrange(preds, Prediction,Water)# sorts data set by site,month,year
plot(otolith~water, Ba_reg_data)# plots measured water and otolith values on response scale
lines(preds$Water,preds$Prediction, col="red",lty=2,lwd=3)# adds fitted line
lines(preds$Water,preds$LCL,col="blue",lty=2,lwd=3)# adds UCL line
lines(preds$Water,preds$UCL,  col="blue",lty=2,lwd=3)# adds LCL line

#####Prediction Intervals for otolith to water- link scale#######
# #oto_inverse =  data.frame(x$linkfun(Ba_reg_data$otolith))
# #colnames(oto_inverse) = c("inverse_oto")# give variables logical names
# #Ba_reg_data = data.frame(Ba_reg_data, oto_inverse)
# preds_link = predict.glm( Ba_oto_water, newdata = Ba_reg_data, type = "link", se.fit = TRUE)# use the knowns regression to make predictions from  5th and 95th percentile water, also provides  std. error
# critval <- 1.96 # critical value for approx 95% CI
# upr <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction
# lwr <- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction
# fit <- preds_link$fit# returns fited value
# preds_link = data.frame(fit,lwr,upr,Ba_reg_data$site, Ba_reg_data$water, Ba_reg_data$otolith)# puts predictions, CI, site ,and measured otolith and water values in a single dataframe
# colnames(preds_link) = c("Prediction","LCL", "UCL", "Site","Water","Otolith")# give variables logical names
# preds_link = arrange(preds_link, Prediction,Water)# sorts data set by site,month,year
# plot(inverse_oto~water, Ba_reg_data)# plots measured water and otolith values on link scale
# lines(preds_link$Water,preds_link$Prediction, col="red",lty=2,lwd=3)# adds fitted line
# lines(preds_link$Water,preds_link$LCL,col="blue",lty=2,lwd=3)# adds LCL line
# lines(preds_link$Water,preds_link$UCL,  col="blue",lty=2,lwd=3)# adds UCL line
# remove(x, critval,fit,lwr,r2,upr)# cleans up work environment

#################Wilcoxon model  code included to provide non-parametric example ############## 
#kruskal.test(value~site, Ba_data)
#pairwise.wilcox.test(Ba_data$value, Ba_data$site,p.adjust.method = "bonferroni" )