########Initial workspace setup#######
setwd("c:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory 
SR_data = data.frame(read.csv(file="c:/Users/olive/Google Drive/Alex_data_and_SAS_files/Sr_Code_data.csv"))#calls dataset 
#SR_data = droplevels(SR_data[-which(SR_data$site == "utrib"),])
#SR_data$value = log(SR_data$value)
options(scipen=999)


########## Calls in Packages####### 
library(car) #regession utilities
library(lme4)# linearmodels 
library(lmerTest)# added utility to linear models 
library(lsmeans)
library(dplyr)# data manipulator 
library(moments)# used to calculate skewness and kurtosis 
library(e1071)



######### arrange the data############
SR_data$month = factor(SR_data$month, levels = c("may", "june", "july", "aug", "sept", "oct"))# forces the ordering(levels) of month to be logical rather than alphabetical
SR_data = arrange(SR_data,site,year,month)# sorts data set by site,month,year

#########User specified functions########## 
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error 

##########Basic descriptive statistics-my R equivalent to SAS proc univariate ######### 
percentile = c(0.05,0.95)# Percentiles of interest 
Descriptive_stats = summarise(group_by(SR_data,site), 
                              Mean = mean(value), 
                              N = length(value), 
                              SD = sd(value), 
                              SE = se(value), 
                              Median = median(value), 
                              Skewness = skewness(value, type = 2), 
                              Kurtosis = kurtosis(value, type = 2), 
                              Skewmness = skewness(value), 
                              Kurtosis = kurtosis(value), 
                              Fifth_percentile= quantile(value, probs = percentile[1], type = 2), 
                              Ninety_Fifth_percentile= quantile(value, probs = percentile[2], type = 2)) 
##########Test assumptions of normality and homoscedasticity- mix of proc univariate and glm bartlett test#########
temp_norm = shapiro.test(SR_data$value)# normality test 
P = data.frame(temp_norm$p.value) 
W = data.frame(temp_norm$statistic) 
Shapiro_wilks_norm = data.frame(W,P)# puts normality test into dataframe
colnames(Shapiro_wilks_norm) = c("W","P")# give variables logical names

temp_var = bartlett.test(value ~ site, data = SR_data) #Bartletts homogeniety of variance test r; requires atleast 2 values per group 
P_B = data.frame(temp_var$p.value)
K = data.frame(temp_var$statistic)
Bartlett_Homogen_var = data.frame(K, P_B)# puts homogeniety test into dataframe
colnames(Bartlett_Homogen_var) = c("K","P")# give variables logical names
remove(P,W,temp_norm,temp_var,K,P_B)# cleans up work environment to point

#################Linear model##############
Sr_site_lm = glm(value ~ site, SR_data, family = "gaussian")
summary(Sr_site_lm)
glm_sum = summary(Sr_site_lm)

msr = (glm_sum$null.deviance - glm_sum$deviance)/(glm_sum$df.null - glm_sum$df.residual)
mse = glm_sum$deviance/glm_sum$df.residual
Model_F_stat = data.frame(msr/mse)
P_or_F = data.frame(2 * pf(q=Model_F_stat, df1=(glm_sum$df.null - glm_sum$df.residual), df2=(glm_sum$df.residual), lower.tail=FALSE))
Model_F_test = data.frame(Model_F_stat,P_or_F)

confint(Sr_site_lm, level=0.95)
lsmeans::lsmeans(Sr_site_lm,pairwise~site)

#####Prediction Intervals from glm#######
preds = predict.glm(Sr_site_lm,type="link", se.fit = TRUE)

#################Wilcoxon model##############
kruskal.test(value~site, SR_data)
pairwise.wilcox.test(SR_data$value, SR_data$site,p.adjust.method = "bonferroni" )

