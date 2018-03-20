setwd("c:/Users/olive/Google Drive/Alex_data_and_SAS_files")#sets working directory
SR_data = data.frame(read.csv(file="c:/Users/olive/Google Drive/Alex_data_and_SAS_files/Sr_Code_data.csv"))#calls dataset
SR_data = SR_data[order(SR_data$site),]#sorts data set by site
########## Calls in Packages#######
library(lme4)#linearmodels
library(lmerTest)#added utility to linear models
library(dplyr)#data manipulator
library(moments)# used to calculate skewness and kurtosis

#########User specified functions##########
se =  function(x) sd(x)/(sqrt(length(x)))# calculates standard error


##########Basic Descriptive Statistics-My R equivalent to SAS proc univariate #########

percentile = c(0.05,0.95)# Percentiles of interest
Descriptive_stats = summarise(group_by(SR_data,site),
                    Mean = mean(value),
                    N = length(value),
                    SD = sd(value),
                    SE = se(value),
                    Median = median(value),
                    Mode = mode(value),
                    Skewmness = skewness(value),
                    Kurtosis = kurtosis(value),
                    Fifth_percentile= quantile(value, probs = percentile[1]))

temp_norm = shapiro.test(SR_data$value)# normality test
P = data.frame(temp_norm$p.value)
W = data.frame(temp_norm$statistic)
Shapiro_wilks_norm = data.frame(W,P)
colnames(Shapiro_wilks_norm) = c("W","P")
remove(P,W,temp_norm)
#homo_var_test = bartlett.test(value ~ site, data = SR_data) #Bartletts homogeniety of variance test remove "#" to use; require 2 values per group

#####Linear model