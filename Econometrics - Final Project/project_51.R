# setup -------------------------------------------------------------------

# general setup
rm(list=ls()) # clear environment
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default

# make sure to install before loading
library(tidyverse) 
library(modelsummary) # produce regression's output to word.
library(lmtest)
library(sandwich)
library(dplyr)
library(car)
library(plm)

library(lfe) 
library(stargazer)
library(knitr)

# set\change working directory - make sure data is in same folder!
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

# reproducibility
set.seed(1)

options(na.action = "na.exclude")

# PART B -----------------------------------------------------------------------------------------------------
## load the data
mydata <- read.csv("term_paper_data_did.csv")

# Question 6 -------------------------------------------------------------------------------------------------

# Filter the data for individuals born in 1948 and year 1985
mydata_1948 <- subset(mydata, year == 85 & d53 == 0)

# estimate model: lnearn = b0 + b1 * treat_muni + u
model1_1948 <-lm(lnearn ~ treat_muni, data=mydata_1948) 
summary(model1_1948) # prints a summary of results

# estimate model: yearsch = b0 + b1 * treat_muni + u
model2_1948 <-lm(yearsch ~ treat_muni, data=mydata_1948) 
summary(model2_1948) # prints a summary of results


# Question 8 -------------------------------------------------------------------------------------------------

mydata_1953 <- subset(mydata, year == 85 & d53 == 1)

# estimate model: lnearn = b0 + b1 * treat_muni + u
model1_1953 <-lm(lnearn ~ treat_muni, data=mydata_1953) 
summary(model1_1953) # prints a summary of results

# estimate model: yearsch = b0 + b1 * treat_muni + u
model2_1953 <-lm(yearsch ~ treat_muni, data=mydata_1953) 
summary(model2_1953) # prints a summary of results

# Question 10 -------------------------------------------------------------------------------------------------

# estimate model: lnearn = b0 + b1 * yearsch + b2 * treat_muni + b3 * hiab3 + b4 * hife + u
model10 <-lm(lnearn ~ yearsch + treat_muni + hiab3 + hife, data=mydata_1948) 
summary(model10) # prints a summary of results

# Question 12 -------------------------------------------------------------------------------------------------

#Let's say we have a variable with many levels, e.g.: Level
#We want to run regression with dummies for EACH ONE of the level types
#The easiest approach is the factor variables approach
class(mydata_1948$level)
mydata_1948$level_f <- factor(mydata_1948$level)
class(mydata_1948$level_f)

# estimate model: lnearn = b0 + b1 * level_f + b2 * treat_muni + b3 * hiab3 + b4 * hife + u
model12 <-lm(lnearn ~ level_f + treat_muni + hiab3 + hife, data=mydata_1948) 
summary(model12) # prints a summary of results

# PART C ------------------------------------------------------------------------------------------------------
# Question 14 -------------------------------------------------------------------------------------------------

mydata_dd <- subset(mydata, year == 85)

first_mean_48 <- mean(mydata_dd$yearsch[mydata_dd$treat_muni == 0 & mydata_dd$d53 == 0])
first_mean_48
second_mean_48 <- mean(mydata_dd$yearsch[mydata_dd$treat_muni == 1 & mydata_dd$d53 == 0])
second_mean_48
first_mean_53 <- mean(mydata_dd$yearsch[mydata_dd$treat_muni == 0 & mydata_dd$d53 == 1])
first_mean_53
second_mean_53 <- mean(mydata_dd$yearsch[mydata_dd$treat_muni == 1 & mydata_dd$d53 == 1])
second_mean_53

diff_48_1 <- second_mean_48 - first_mean_48
diff_48_1
diff_53_1 <- second_mean_53 - first_mean_53
diff_53_1
dd_14_1 <- diff_53_1 - diff_48_1
dd_14_1

diff_48_2 <- first_mean_53 - first_mean_48 
diff_48_2
diff_53_2 <- second_mean_53 - second_mean_48 
diff_53_2
dd_14_2 <- diff_53_2 - diff_48_2
dd_14_2



# Question 15 -------------------------------------------------------------------------------------------------

# estimate model: yearsch = b0 + b1 * treat_muni + b2 * d53 + b3 * treat_muni:d53 + u
model_dd <-lm(yearsch ~ treat_muni + d53 + treat_muni:d53, data=mydata_dd) 
summary(model_dd) # prints a summary of results


# Question 17 -------------------------------------------------------------------------------------------------

#generate residuals 
mydata_dd$resids <- model_dd$residuals
mydata_dd$resids2 <- mydata_dd$resids^2

#generate yhat
mydata_dd$yhat <- model_dd$fitted.values

plot(y = mydata_dd$resids, x = mydata_dd$treat_muni)
plot(y = mydata_dd$resids, x = mydata_dd$d53)
plot(y = mydata_dd$resids, x = mydata_dd$yhat)
plot(y = mydata_dd$resids^2, x = mydata_dd$yhat)

#BP Test
# estimate model: resids2 = b0 + b1 * treat_muni + b2 * d53 + b3 * treat_muni:d53 + u
model_BP <-lm(resids2 ~ treat_muni + d53 + treat_muni:d53, data=mydata_dd) 
summary(model_BP) # prints a summary of results

#re-estimate the model with white's robust standard errors
coeftest(model_dd, vcov = vcovHC(model_dd, type="HC1"))


# Question 18 -------------------------------------------------------------------------------------------------

# estimate model: yearsch = b0 + b1 * treat_muni + b2 * d53 + b3 * treat_muni:d53 + b4 * female + b5 * hiab3 + b6 * hife + u
model_dd_18 <-lm(yearsch ~ treat_muni + d53 + treat_muni:d53 + female + hiab3 + hife, data=mydata_dd) 
summary(model_dd_18) # prints a summary of results

#generate residuals 
mydata_dd$resids_18 <- model_dd_18$residuals
mydata_dd$resids2_18 <- mydata_dd$resids_18^2

#BP Test
# estimate model: resids2_18 = b0 + b1 * treat_muni + b2 * d53 + b3 * treat_muni:d53 + b4 * female + b5 * hiab3 + b6 * hife + u
model_BP_18 <-lm(resids2_18 ~ treat_muni + d53 + treat_muni:d53 + female + hiab3 + hife, data=mydata_dd) 
summary(model_BP_18) # prints a summary of results

#re-estimate the model with white's robust standard errors
coeftest(model_dd_18, vcov = vcovHC(model_dd_18, type="HC1"))

# Question 19 -------------------------------------------------------------------------------------------------

linearHypothesis(model_dd_18, c("hiab3= 0", "hife=0", "female=0"))

# Question 22 -------------------------------------------------------------------------------------------------

#fixed effect by female + hiab3 + hife + year, cluster by year
model_dd_22 <- felm(lnearn ~ treat_muni*d53|female + hiab3 + hife + year| 0 |year, data=mydata) 
summary(model_dd_22) # prints a summary of results

# Question 23 -------------------------------------------------------------------------------------------------

males <- subset(mydata, female == 0)
females <- subset(mydata, female == 1)

males_high_edu_father <- subset(males, hife == 1)
females_high_edu_father <- subset(females, hife == 1)

males_low_edu_father <- subset(males, hife == 0)
females_low_edu_father <- subset(females, hife == 0)


model_dd_23_males <- felm(lnearn ~ treat_muni*d53| hiab3 + hife + year| 0 |year, data=males) 
model_dd_23_females <- felm(lnearn ~ treat_muni*d53| hiab3 + hife + year| 0 |year, data=females) 

model_dd_23_males_hef <- felm(lnearn ~ treat_muni*d53| hiab3 + year| 0 |year, data=males_high_edu_father) 
model_dd_23_females_hef <- felm(lnearn ~ treat_muni*d53| hiab3 + year| 0 |year, data=females_high_edu_father) 

model_dd_23_males_lef <- felm(lnearn ~ treat_muni*d53| hiab3 + year| 0 |year, data=males_low_edu_father) 
model_dd_23_females_lef <- felm(lnearn ~ treat_muni*d53| hiab3 + year| 0 |year, data=females_low_edu_father) 


model_list <- list(model_dd_23_males, model_dd_23_females, model_dd_23_males_hef, model_dd_23_females_hef, model_dd_23_males_lef, model_dd_23_females_lef)

stargazer(model_list, type = 'text', no.space=T, keep.stat = c('n', 'rsq'))

stargazer_cols = c("(1) - males", '(2) - females', '(3) - males_high_edu_father', '(4) - females_high_edu_father', '(5) - males_low_edu_father', '(6) - females_low_edu_father')
print(stargazer_cols)



