## week one

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots

g <- read.csv("data/simulated-HF-mort-data.csv")
head(g)

gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

## plot KM
km_fit <- survfit(Surv(fu_time, death) ~ 1)

plot(km_fit)

##
summary(km_fit, times = c(1:7,30,60,90*(1:10)))

## The “times” argument gives us control over what time periods we want to see. 
# The above code asks for output every day for the first week, then at 30, 60 and 90 days, 
# and then every 90 days thereafter. Here’s the output:

## now lets fit it with gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 

plot(km_gender_fit)

## run log rank test
survdiff(Surv(fu_time, death) ~ gender, rho=0) 

# With rho = 0, which is the default so we don’t need to write this bit, 
# it yields the log-rank or Mantel-Haenszel test. When you run the above, you should get this output:

#now check for 65 plus age 
age_65plus <- ifelse(g[,"age"]>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea

survdiff(Surv(fu_time, death) ~ age_65plus, rho=0)
