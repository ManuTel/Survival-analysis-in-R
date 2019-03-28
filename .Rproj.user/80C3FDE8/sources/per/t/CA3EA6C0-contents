library(survminer)
library(survival)

g <- read.csv("data/simulated-HF-mort-data.csv")
head(g)
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) # take variables straight from g
# in this model, ethnicgroup is treated as continuous variable, therefore it gives only one coefficient
summary(cox)


ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time <- g[,"fu_time"]
death <- g[,"death"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

## week 3
age         <- g[,"age"]
gender      <- g[,"gender"]
copd        <- g[,"copd"]
prior_dnas  <- g[,"prior_dnas"]
ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time     <- g[,"fu_time"]
death       <- g[,"death"]

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)

summary(cox)
summary(prior_dnas)
table(prior_dnas)
quintile <- factor(g[,"quintile"])

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

### to check the cox proportional hazards assumption. the p-value should not be significant on residual plot
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model

temp <- cox.zph(fit)# apply the cox.zph function to the desired model

print(temp) # display the results

plot(temp) # plot the curves
ggcoxzph(temp) 

# Technically speaking, the function cox.zph() correlates for each predictor the corresponding set of 
# scaled Schoenfeld residuals with time, to test for independence between residuals and time. 

res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 
ggcoxdiagnostics(res.cox, type = "deviance", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

## Another issue is whether any continuous variables that you assume to have a linear relation with the outcome 
## actually do have a linear relation. If you fit age as a single term in the model, then that’s what you’re assuming. 
## The martingale residual is used to test this assumption:
fit2 <- Surv(fu_time, death) ~ age + log(age) + sqrt(age)
ggcoxfunctional(fit2, data = g) # note we must specify original dataframe 

# Martingale residuals near 1 represent individuals that “died too soon”
# Large negative values correspond to individuals that “lived too long”
# The plots should give you nice straight line if the assumption is valid.

## Let’s go through how to include this interaction term and test whether it’s statistically significant. 
## For mathematical reasons, you can’t just include the follow-up time itself as part of the interaction but 
## instead need to transform it. The easiest way to do this in R is via the “tt” function (short for “time transform”):
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function 
summary(fit) 

## This output agrees with the earlier approach and says that the interaction between gender and (transformed) time 
## is not statistically significant, i.e. there’s no apparent violation of the proportionality assumption. 
## Again, good news. The p-value from this approach (about 0.5) isn’t the same as that from the earlier one because 
## the methods are different, though it’s always nice when they give the same message!


  
  
