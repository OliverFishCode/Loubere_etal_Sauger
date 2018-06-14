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
levenetest_origin = leveneTest(loglin_data$count,loglin_data$origin) #levene homogeniety of variance test r; requires atleast 2 values per group 
levenetest_pool = leveneTest(loglin_data$count,loglin_data$pool) #levene test is used instead of bartletts because its robust to non-normal data

#################loglinear Linear Model- includes f-test for overall model##############
Sauger_loglin = glm(count ~ pool + origin + origin*pool, loglin_data, family = "poisson")# linear model to test differences in value by site
Sauger_null = glm(count ~ 1, loglin_data, family = "poisson")
loglin_model_p = with(anova(Sauger_null,Sauger_loglin),pchisq(Deviance,Df,lower.tail=FALSE)[2]) 
overdispersion_test = dispersiontest(Sauger_loglin)#overdispersion test,you can also divide resid deviance by resid df
summary(Sauger_loglin)# prints summary of linear coefficients and significance 
loglin_summary =broom::tidy(Sauger_loglin)# summary in a pretty dataframe
CL_lm = broom::confint_tidy(Sauger_loglin, conf.level=0.95)# 95% clm on linear coefficients
temp_eemeans = emmeans(Sauger_loglin,"origin", by="pool")# creats marginal means object
tukey_pairwise_lm = broom::tidy(contrast(temp_eemeans, by= "pool",method="pairwise", adjust = "tukey" ))# tukeys pairwise tests in dataframe
cld_sum = cld(temp_eemeans)
plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model
loglin_summary = data.frame(lm_summary,CL_lm)# combines linear coeffecient summary and CLM into same dataframe
remove(CL_lm,Sauger_loglin)# cleans up work environment to point


#########compare multi-comp outcomes#######
# plot(temp_eemeans, comparisons =TRUE)# Pairwise comaparisons plot for the basic linear model
# plot(temp_eemeans2, comparisons =TRUE)# Pairwise comaparisons plot for the temporal autocorrelation linear model
# cld(temp_eemeans)# compact letter display (tukey groupings) for basic linear model
# cld(temp_eemeans2)# compact letter display (tukey groupings) for temporal autocorrelation linear model
# remove(temp_eemeans2,temp_eemeans,temporal_CL_lm,Sr_temporal_lm,temp_sum)# cleans up work environment to point
# 
# 


