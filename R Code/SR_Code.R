########Initial workspace setup#######
setwd("c:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
SR_data = data.frame(read.csv(file="c:/Users/olive/Google Drive/Alex_data_and_SAS_files/Sr_Code_data.csv"))#calls dataset 
#SR_data = droplevels(SR_data[-which(SR_data$site == "utrib"),])# removes utrib to test effects on normality and homogeniety
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



######### arrange the data############
SR_data$month = factor(SR_data$month, levels = c("may", "june", "july", "aug", "sept", "oct"))# forces the ordering(levels) of month to be logical rather than alphabetical
SR_data$year= factor(SR_data$year, levels = c("2013", "2014", "2015", "2016", "2017"))# forces the ordering(levels) of year and makes it descrete rather than continuous

SR_data = arrange(SR_data,site,year,month)# sorts data set by site,month,year

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 

##########Basic descriptive statistics-my R equivalent to SAS proc univariate ######### 
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats = summarise(group_by(SR_data,site),# applys following statistics by group 
                              Mean = mean(value), 
                              N = length(value), 
                              SD = sd(value), 
                              SE = se(value), 
                              Median = median(value), 
                              Skewness = skewness(value, type = 2), 
                              Kurtosis = kurtosis(value, type = 2), # type 2 corrisponds to the same estimations method used by sas
                              Fifth_percentile= quantile(value, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(value, probs = percentile[2], type = 2)) 
remove(se, percentile)# cleans up work environment to point

########Add 5th and 95th percentile values to data for predictions##########
value = Descriptive_stats$Fifth_percentile
site = Descriptive_stats$site
year = ""
month = ""
Site_low = data.frame(value, site, month, year)
Site_low$site = plyr::revalue(Site_low$site,c("cumb"="lcumb", "mmr"="lmmr", "ohio"="lohio", "tenn"="ltenn", "twr"="ltwr", "union"="lunion", "utrib"="lutrib", "wab"="lwab"))
value = Descriptive_stats$Ninety_Fifth_percentile
Site_high = data.frame(value, site, month, year)
Site_high$site = plyr::revalue(Site_high$site,c("cumb"="ucumb", "mmr"="ummr", "ohio"="uohio", "tenn"="utenn", "twr"="utwr", "union"="uunion", "utrib"="uutrib", "wab"="uwab"))
SR_data_bounds = rbind(SR_data, Site_high, Site_low)
remove(Site_high,Site_low, month, site, value,year)# cleans up work environment to point

##########Test assumptions of normality and homoscedasticity- mix of proc univariate and glm bartlett test#########
temp_norm = shapiro.test(SR_data$value)# normality test 
P = data.frame(temp_norm$p.value)# temporary variable to contain P
W = data.frame(temp_norm$statistic)# temporary variable to contain w statistic
Shapiro_wilks_norm = data.frame(W,P)# puts normality test output into dataframe
colnames(Shapiro_wilks_norm) = c("W","P")# give variables logical names

temp_var = bartlett.test(value ~ site, data = SR_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)# temporary variable to contain P
K = data.frame(temp_var$statistic)# temporary variable to contain k statistic
Bartlett_Homogen_var = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_var) = c("K","P")# give variables logical names
remove(P,W,temp_norm,temp_var,K,P_B)# cleans up work environment to point

#################Linear Model##############
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
Model_F_test = data.frame(Model_F_stat,P_or_F)# puts F test in dataframe
CL_lm = broom::confint_tidy(Sr_site_lm, conf.level=0.95)# 95% clm on linear coefficients
temp_eemeans = emmeans(Sr_site_lm,"site")# creats marginal means object
tukey_pairwise = broom::tidy(contrast(temp_eemeans, method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
lm_summary = data.frame(lm_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
<<<<<<< HEAD
remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,temp_eemeans,CL_lm)# cleans up work environment to point
=======
remove(glm_sum,P_or_F,Model_F_stat,msr,mse,null_df,resid_df,temp_eemeans,CL_lm)
>>>>>>> master

############Linear Mixed Model############

#######Linear Mixed Model with AR(1) R-side error correction########

#####Prediction Intervals for otolith to water *****place holder#######
preds = predict.glm( Sr_site_lm,type="link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
preds = data.frame(fit,upr,lwr, SR_data$site)

#################Wilcoxon model  code included to provide non-parametric example ############## 
#kruskal.test(value~site, SR_data)
#pairwise.wilcox.test(SR_data$value, SR_data$site,p.adjust.method = "bonferroni" )

