# Load packages 
require(survival)  
require(ggplot2)  
require(survminer) 
require(ggfortify) 

# Load dataset 
g <- read.csv(file = "./data/simulated-HF-mort-data.csv") 


# Define variables 
gender      <- factor(g[,"gender"]) 
fu_time     <- g[,"fu_time"]  
death       <-  g[,"death"] 
age         <- g[,"age"] 
copd        <- factor(g[,"copd"]) 
ethnicgroup <- factor(g[,"ethnicgroup"]) 
quintile    <- factor(g[,"quintile"]) 
ihd         <- factor(g[,'ihd']) 
valvular    <- factor(g[,'valvular_disease']) 
pvd         <- factor(g[,'pvd']) 
stroke      <- factor(g[,'stroke']) 
pneumonia   <- factor(g[,'pneumonia']) 
renal       <- factor(g[,'renal_disease']) 
ca          <- factor(g[,'cancer']) 
mets        <- factor(g[,'metastatic_cancer']) 
mental_health <- factor(g[,'mental_health']) 
ht          <- factor(g[,"hypertension"]) 
cog_imp     <- factor(g[,"senile"]) 
prior_dnas  <- g[,"prior_dnas"] 



# Plotting a Kaplan-Meier curve 
###################### 

# 1. Generate the survival curve 
km_fit <- survfit(Surv(fu_time, death) ~ 1) 

# 2b. Alternative plot with ggplot2 
autoplot(km_fit) + theme_bw() # theme_bw() is a predesigned "theme" which makes the plot prettier 

# Output the probability of survival at certain times after hospital admission 
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

# Plotting a Kaplan-Meier curve by gender 
###################### 

# 1. Generate the survival curve 
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 

# 2. Plot the curve 
plot(km_gender_fit)

# 2b. Alternative plot with ggplot2 
autoplot(km_gender_fit) + theme_bw() 

# Perform log rank test to see whether survival varies by gender 
survdiff(Surv(fu_time, death) ~ gender, rho = 0) 

# Testing whether those over the age of 65 have different survival to those under it 
###################### 

# 1. Dichotomise age into categorical (binary in this case) variable 
age_65plus <- ifelse(g[,'age']>=65, 1, 0) 

# 2. Perform log rank test 
survdiff(Surv(fu_time, death) ~ age_65plus, rho = 0) 

# Plot survival curve by age above or below 65 
###################### 

# 1. Generate survival curve 
km_old_fit <- survfit(Surv(fu_time, death) ~ age_65plus) 

# 2. Plot 
plot(km_old_fit) 

# 2b. Alternative plot in ggplot2 
autoplot(km_old_fit) + theme_bw()

###################### 

# Run Cox regression model with age as predictor (continuous variable) 
###################### 

# 1. Generate model 
cox <- coxph(Surv(fu_time, death) ~ age, data = g) 

# 2. Summarise model 
summary(cox) 

###################### 



# Run Cox regression model with quintile as predictor (categorical variable) 
# Changing the reference group to first quintile 
# Removing the zero quintile altogether 
###################### 


# 1. Summarise the variable 
table(quintile, exclude = NULL) 

## quintile 
##    0    1    2    3    4    5 <NA>  
##    4  138  205  211  220  216    6 

# 2. Check levels 
levels(quintile) 

## [1] "0" "1" "2" "3" "4" "5" 

# 3. Generate model 
cox <- coxph(Surv(fu_time, death) ~ quintile) # warning 

## Warning in fitter(X, Y, strats, offset, init, control, weights = weights, : 
## Loglik converged before variable 1,2,3,4,5 ; beta may be infinite. 

# 4. Summarise model 
summary(cox) 

# 5. Make the first quintile the reference group 
quintile <- relevel(quintile, ref = "1") 

# 6. Regenerate and summarise model 
cox <- coxph(Surv(fu_time, death) ~ quintile) # warning 

## Warning in fitter(X, Y, strats, offset, init, control, weights = weights, : 
## Loglik converged before variable 1 ; beta may be infinite. 

summary(cox) # still an issue where quintile = 0 

# 7. Inspecting quintile variable 
table(quintile, g$death) # Only 4 entries for quintile = 0 and 100% didn't die 

# 8. Removing quintile = 0 entries as there are only 4 of them 
quintile_5groups <- quintile 
quintile_5groups[quintile_5groups == 0] <- NA # set the zeroes to missing 
quintile_5groups <- factor(quintile_5groups) # this removes 0 as a level as it is an empty category 

# 9. Regenerating the model and summarising 
cox <- coxph(Surv(fu_time, death) ~ quintile_5groups) 
summary(cox) # still an issue where quintile = 0 

###################### 

# Run Cox regression model with ethnic group as predictor (categorical variable) 
# Including missing values as another category 
###################### 

# 1. Summarise variable 
table(ethnicgroup, exclude = NULL) 

## ethnicgroup 
##    1    2    3    9 <NA>  
##  889   17   34   17   43 

# 2. Generate and summarise model 
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 

# 3. Add another category (8) 
levels(ethnicgroup) <- c(levels(ethnicgroup),"8")  

# 4. Redefine NA as another group, 8 
ethnicgroup[is.na(ethnicgroup)] <- "8" 

# 5. Regenerate and summarise model 
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 

###################### 



# Investigating our variables in order to best perform a Cox model with multiple predictors 
# Checking for missing values 
# Running a multiple Cox regression 
###################### 

# 1. Summarising age 
summary(g$age) # no NAs 

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
##   29.00   73.00   80.00   78.73   87.00  102.00 

hist(g$age) 
# 2. Gender 
gender_table <- table(gender, exclude = NULL) 
addmargins(gender_table) # no NAs 

## gender 
##    1    2  Sum  
##  548  452 1000 

round(100 * prop.table(gender_table), digits = 1) # Percentages rounded to 1 decimal place 

## gender 
##    1    2  
## 54.8 45.2 

# 3. Chronic Obstructive Pulmonary Disease (COPD) 
copd_table <- table(copd, exclude = NULL)  
addmargins(copd_table) # no NAs 

## copd 
##    0    1  Sum  
##  758  242 1000 

round(100 * prop.table(copd_table), digits = 1) # Percentages rounded to 1 decimal place 

## copd 
##    0    1  
## 75.8 24.2 

# 4. Prior OPD appointments missed  
prior_dnas_table <- table(prior_dnas, exclude = NULL)  
addmargins(prior_dnas_table) # no NAs 

## prior_dnas 
##    0    1    2    3    4    5    6    7    8   10  Sum  
##  732  156   50   34   17    3    3    2    1    2 1000 

round(100 * prop.table(prior_dnas_table), digits = 1) # Percentages rounded to 1 decimal place 

## prior_dnas 
##    0    1    2    3    4    5    6    7    8   10  
## 73.2 15.6  5.0  3.4  1.7  0.3  0.3  0.2  0.1  0.2 

# 5. Ethnic group 
ethnicgroup_table <- table(ethnicgroup, exclude = NULL)  
addmargins(ethnicgroup_table) # 4.3% NA 

## ethnicgroup 
##    1    2    3    9    8  Sum  
##  889   17   34   17   43 1000 

round(100 * prop.table(ethnicgroup_table), digits = 1) # Percentages rounded to 1 decimal place 

## ethnicgroup 
##    1    2    3    9    8  
## 88.9  1.7  3.4  1.7  4.3 

# 6. Generate and summarise model 
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup) 
summary(cox) 

###################### 

# Investigating whether the assumptions of the Cox model are being broken 
# Testing for proportional hazards assumption (with gender as predictor variable) 
###################### 

# 1. Generate model fit 
fit <- coxph(Surv(fu_time, death) ~ gender) 

# 2. Apply the test to the model 
temp <- cox.zph(fit)     

# 3. Display results 
print(temp) 

##            rho chisq     p 
## gender2 0.0493  1.19 0.275 

# 4. Plot the curves 
plot(temp)     

# 4b. Alternative plot in ggplot 
ggcoxzph(temp) 

###################### 

# Generating other diagnostic plots for Cox Proportional Hazards model 
###################### 

# 1. Define model 
res.cox <- coxph(Surv(fu_time, death) ~ age) 

# Generate diagnostic plots 

# 2. Plotting the estimated changes in the regression coefficients on deleting each patient 
ggcoxdiagnostics(res.cox, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

# 3. Plotting deviance residuals 
ggcoxdiagnostics(res.cox, type = "deviance", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

# 4. Plotting Martingale residuals 
fit <- coxph(Surv(fu_time, death) ~ age + log(age) + sqrt(age)) 
ggcoxfunctional(fit, data = g) # note we must specify original dataframe 

###################### 

# Testing proportionality assumption 
# Testing for a statistical relationship between gender and time 
###################### 

# 1. Generate model with time-transform function (tt) 
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender))  

# 2. Summarise 
summary(fit) 

###################### 

# Backwards elimination to choose predictors for Cox regression 
###################### 


# 1. Run the full model with all of your predictors 
cox <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + 
               valvular + pvd + stroke + copd + pneumonia + ht + renal + 
               ca + mets + mental_health + cog_imp) 
summary(cox) 


# 2. Run the model with only significant predictors 
cox <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp) 
summary(cox) 

# 3. Test proportionality assumption on these predictors 
cox.zph(cox)  

##                 rho  chisq     p 
## age        -0.04409 0.9098 0.340 
## gender2     0.05474 1.4133 0.235 
## valvular1  -0.04565 1.0202 0.312 
## pneumonia1 -0.04397 0.9540 0.329 
## mets1       0.00615 0.0184 0.892 
## cog_imp1    0.05985 1.8140 0.178 
## GLOBAL           NA 5.4044 0.493 
