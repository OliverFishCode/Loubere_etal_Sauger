########Initial workspace setup#######
setwd("C:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
loglin_data = data.frame(read.csv(file="C:/Users/olive/Google Drive/Alex_data_and_SAS_files/loglin_data.csv"))#calls loglin dataset 
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
library(AER)

######### arrange the data############
colnames(loglin_data) = c("count","pool","year","origin")# fixes column names

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 

##########Basic descriptive statistics-my R equivalent to SAS proc univariate ######### 
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats = summarise(group_by(loglin_data,pool),# applys following statistics by group 
                              Mean = mean(count), 
                              N = length(count),# number of observations
                              SD = sd(count),# standard deviation 
                              SE = se(count),# standard error 
                              Median = median(count), 
                              Skewness = skewness(count, type = 2),# type 2 corrisponds to the same estimations method used by sas 
                              Kurtosis = kurtosis(count, type = 2),# type 2 corrisponds to the same estimations method used by sas
                              Fifth_percentile= quantile(count, probs = percentile[1], type = 2),# type 2 corrisponds to the same estimation method used in sas
                              Ninety_Fifth_percentile= quantile(count, probs = percentile[2], type = 2))# type 2 corrisponds to the same estimation method used in sas 
remove(se, percentile)# cleans up work environment to point


##########Test assumptions homoscedasticity- mix of proc univariate and glm bartlett test#########

temp_var = bartlett.test(count ~ origin, data = loglin_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)# temporary variable to contain P
K = data.frame(temp_var$statistic)# temporary variable to contain k statistic
Bartlett_Homogen_origin = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_water) = c("K","P")# give variables logical names

temp_var = bartlett.test(count ~ pool, data = loglin_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)# temporary variable to contain P
K = data.frame(temp_var$statistic)# temporary variable to contain k statistic
Bartlett_Homogen_pool = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_water) = c("K","P")# give variables logical names

remove(K, P_B, temp_var)# cleans up work environment to point

#################Water Linear Model- includes f-test for overall model##############
Sr_site_lm = glm(count ~ pool, loglin_data, family = "poisson")# linear model to test differences in value by site
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
dispersiontest(Sr_site_lm,trafo = 1)#overdispersiontest
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




