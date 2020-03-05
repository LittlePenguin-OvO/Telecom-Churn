####
rm(list = ls())
#setwd("/Users/mac/Desktop/telecom")
library(plyr)
library(corrplot)
library(sampling)
library(caTools)
library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(e1071)
library(tidyr)
telecom <- read.csv("teleco_churn.csv", header = T, na.strings="-1")

#telecom[which(telecom$number_customer_service_calls > 5),"number_customer_service_calls"] = 6
attach(telecom)

### Change our variables
# Correlation of minutes and charge are close to one 
cor1 = cor(telecom$total_day_minutes, telecom$total_day_charge)
cor2 = cor(telecom$total_eve_minutes, telecom$total_eve_charge)
cor3 = cor(telecom$total_night_minutes, telecom$total_night_charge)
cor4 = cor(telecom$total_intl_minutes, telecom$total_intl_charge)
c(cor1, cor2, cor3, cor4)

# Calculate minute per call
telecom$day_minute_per_call = telecom$total_day_minutes/telecom$total_day_calls
telecom$eve_minute_per_call = telecom$total_eve_minutes/telecom$total_eve_calls
telecom$night_minute_per_call = telecom$total_night_minutes/telecom$total_night_calls
telecom$intl_minute_per_call = telecom$total_intl_minutes/telecom$total_intl_calls
telecom[is.na(telecom)] = 0

# Relation between churn and state
state = as.character(state)
state = as.factor(state)
mosaicplot(state~churn, shade = TRUE, main = "Mosaic Plot of Churn VS State")

# Classify state by churn rate
# HI,VA are pretty low yes; CA,NJ,TX,WA are pretty high yes, others are normal
# For state_new; low yes = 0, normal yes = 1, high yes = 2
telecom$state = as.character(telecom$state)
telecom$state_new = telecom$state
for (i in 1:length(telecom$state)){
  if (telecom$state[i] == "HI" | telecom$state[i] == "VA"){
    telecom$state_new[i] = 0
  }
  else if (telecom$state[i] == "CA" | telecom$state[i] == "NJ"| telecom$state[i] == "TX" | telecom$state[i] == "WA"){
    telecom$state_new[i] = 2
  }
  else{
    telecom$state_new[i] = 1
  }
}
mosaicplot(churn~telecom$state_new, shade = TRUE)

# Change categorical variables to factors
telecom$state_new = as.factor(telecom$state_new)
telecom$area_code = as.factor(area_code)
telecom$international_plan = as.factor(telecom$international_plan)
telecom$voice_mail_plan = as.factor(telecom$voice_mail_plan)

attach(telecom)

### Split trainset and testset
set.seed(1111)
spl = sample.split(telecom$state, SplitRatio = 0.8)
data_train = subset(telecom, spl==TRUE)
data_test = subset(telecom, spl==FALSE)


### Mosaic plots
# Churn and area code
area = area_code
area = as.character(area)
area = substr(area, 11,13)
area = as.factor(area)
mosaicplot(area~churn, shade = TRUE, main = "Mosaic Plot of Churn VS Area")

# Churn and state
state = as.character(state)
state = as.factor(state)
mosaicplot(state~churn, shade = TRUE, main = "Mosaic Plot of Churn VS State")

# Churn and voice mail plan
mosaicplot(voice_mail_plan~churn,shade = TRUE)

# Churn and internarional plan
mosaicplot(international_plan~churn, shade = TRUE)


### Box plots
# Churn and account length
# similar in no and yes
boxplot(account_length~churn,main = "Boxplot: churn and account_length")

# Churn and number vmail messages
# in no, 3rd quantile is 20, while in yes, 1st=3rd=median=0
boxplot(number_vmail_messages~churn, main = "Boxplot: churn and number of vmail messages")

# Churn and total day calls
# similar, but no has more potential outliers below 50.
boxplot(total_day_calls~churn,main="Boxplot: churn and total day calls")

# Churn and total day minutes
boxplot(total_day_minutes~churn,main="Boxplot: churn and total day minutes")

# Churn and total day charges
# yes has longer minutes therefore higher charge.
boxplot(total_day_charge~churn, main="Boxplot: churn and total day charges")

# Churn and total evening calls
# similar, no has more potential outliers.
boxplot(total_eve_calls~churn, main="Boxplot: churn and total evening calls")

# Churn and total evening minutes and charge
# yes more minutes therefore higher charge
boxplot(total_eve_minutes~churn,main="Boxplot: churn and total evening minutes")
boxplot(total_eve_charge~churn,main="Boxplot:churn and total evening charges")

# Churn and total night calls
# similar, no has more potential outliers.
boxplot(total_night_calls~churn, main="Boxplot: churn and total night calls")

# Churn and total evening minutes and charge
# yes a little bit more minutes therefore higher charge
boxplot(total_night_minutes~churn,main="Boxplot: churn and total night minutes")
boxplot(total_night_charge~churn,main="Boxplot:churn and total night charges")

# Churn and total international calls
# similar, yes has less international calls
boxplot(total_intl_calls~churn, main="Boxplot: churn and total international calls")

# Churn and total international minutes and charge
# yes a little bit more minutes therefore higher charge
boxplot(total_intl_minutes~churn,main="Boxplot: churn and total international minutes")
boxplot(total_intl_charge~churn,main="Boxplot:churn and total international charges")

# Churn and number of customer service cals
# yes has more customer services calls.
par(mfrow=c(1,1))
boxplot(number_customer_service_calls~churn, main="Boxplot:churn and number of customer service calls")

### Churn and state/voice mail plan/international plan/area code: excel

#################################### MODEL BUILDING##############################################
### Original variables
## The response variable is a categorical variable, so use logistic regression.
# Full model with original variables
fit=glm(churn~.-day_minute_per_call-eve_minute_per_call-night_minute_per_call-intl_minute_per_call-state_new,data=data_train,family=binomial)
summary(fit)

# Stepwise selection
step_fit=step(fit, scope = list(upper=fit))
summary(step_fit)
anova(object = step_fit, test = 'Chisq')

# VIF
# number of vmail messages and voice mail plan has larger vif
# but reasonable, the number of vmail messgae depends on whether have voice 
# mail plan, therefore, must high related
library(car)
vif(step_fit)
step_fit = glm(formula = churn ~ international_plan  + 
      number_vmail_messages + total_day_minutes + total_eve_charge + 
      total_night_minutes + total_intl_calls + total_intl_charge + 
      number_customer_service_calls, family = binomial, data = data_train)
vif(step_fit)

# Esimtated probabilities 
prob <- predict(step_fit, newdata=data_train, type = 'response')
table(data_train$churn, prob > 0.5)

# Odds ratio
OR_CI=exp(confint(step_fit))
cbind(exp(coef(step_fit)),OR_CI)

# ROC and error measure
library(InformationValue)
# detach('package:caret')
data_train$churn_new <- 0
data_train$churn_new[data_train$churn == "yes"] <- 1
# threshold = 0.5
plotROC(data_train$churn_new,prob)
sensitivity(data_train$churn_new,prob)
specificity(data_train$churn_new,prob)
confusionMatrix(data_train$churn_new,prob)
misClassError(data_train$churn_new,prob)

# another version of ROC plot
library(pROC)
roc_curve <- roc(data_train$churn_new,prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = , mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+ labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')
## AUC =0.83 model is ok

# Double density plot
temp_train = cbind(data_train,prob)
ggplot(temp_train, aes( prob, color = as.factor(churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's estimated probabilities" ) 

# Confusion Matrix plot
source("unbalanced_function.R")
cm_info <- ConfusionMatrixInfo( data = temp_train, predict = "prob", 
                                actual = "churn_new", cutoff = 0.32 )
cm_info$plot

# Optimal cutoff by considering cost
cost_fp <- 10
cost_fn <- 20
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

# error measure again
detach('package:caret')
sensitivity(data_train$churn_new,prob,threshold = roc_info$cutoff)
specificity(data_train$churn_new,prob,threshold = roc_info$cutoff)
confusionMatrix(data_train$churn_new,prob,threshold = roc_info$cutoff)
misClassError(data_train$churn_new,prob,threshold = roc_info$cutoff)

### Modified variables
fit_new_full = glm(churn ~ state_new + account_length + area_code + international_plan + voice_mail_plan + 
                     number_vmail_messages + total_day_minutes + total_day_calls + day_minute_per_call+ total_eve_minutes + total_eve_calls + eve_minute_per_call+ 
                     total_night_minutes + total_night_calls + night_minute_per_call + total_intl_minutes + total_intl_calls + intl_minute_per_call + 
                     number_customer_service_calls,data=data_train,family=binomial)
summary(fit_new_full)

# Stepwise selection
step_fit_new = step(fit_new_full, scope = list(upper=fit_new_full))
summary(step_fit_new)
fit_new = glm(formula = churn ~ state_new + international_plan + voice_mail_plan + 
      number_vmail_messages + total_day_minutes + total_eve_minutes +  total_night_minutes + 
      total_intl_minutes + intl_minute_per_call + number_customer_service_calls, 
    family = binomial, data = data_train)
anova(step_fit_new, fit_new, test = 'Chisq')
step_fit_new = fit_new

# VIF
# number of vmail messages and voice mail plan has larger vif
# but reasonable, the number of vmail messgae depends on whether have voice 
# mail plan, therefore, must high related
library(car)
vif(step_fit_new)
step_fit_new = glm(formula = churn ~ state_new + international_plan + 
                number_vmail_messages + total_day_minutes + total_eve_minutes +  total_night_minutes + 
                total_intl_minutes + intl_minute_per_call + number_customer_service_calls, 
              family = binomial, data = data_train)

vif(step_fit_new)

# Esimtated probabilities 
prob_new <- predict(step_fit_new, newdata=data_train, type = 'response')
table(data_train$churn, prob_new > 0.5)

# Odds ratio
OR_CI_new =exp(confint(step_fit_new))
cbind(exp(coef(step_fit_new)),OR_CI_new)

# ROC and error measure
# threshold = 0.5
plotROC(data_train$churn_new,prob_new)
sensitivity(data_train$churn_new,prob_new)
specificity(data_train$churn_new,prob_new)
confusionMatrix(data_train$churn_new,prob_new)
misClassError(data_train$churn_new,prob_new)

# another version of ROC plot
library(pROC)
roc_curve <- roc(data_train$churn_new,prob_new)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = , mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+ labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')
# AUC =0.83 model is ok

# Double density plot
temp_train_new = cbind(data_train,prob_new)
ggplot(temp_train_new, aes(prob_new, color = as.factor(churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's estimated probabilities" ) 

# Confusion Matrix plot
source("unbalanced_function.R")
cm_info <- ConfusionMatrixInfo( data = temp_train_new, predict = "prob_new", 
                                actual = "churn_new", cutoff = 0.5 )
cm_info$plot

#Optimal cutoff by considering cost
cost_fp <- 10
cost_fn <- 20
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

#error measure again
detach('package:caret')
sensitivity(data_train$churn_new,prob_new,threshold = roc_info$cutoff)
specificity(data_train$churn_new,prob_new,threshold = roc_info$cutoff)
confusionMatrix(data_train$churn_new,prob_new,threshold = roc_info$cutoff)
misClassError(data_train$churn_new,prob_new,threshold = roc_info$cutoff)

### Choose model
cutoff_1 = 0.32
cutoff_2 = 0.29

predict_1 <- predict(step_fit, newdata=data_test, type = 'response')
predict_2 <- predict(step_fit_new , newdata=data_test, type = 'response')
table_1 = table(data_test$churn, predict_1 > cutoff_1)
table_2 = table(data_test$churn, predict_2 > cutoff_2)

table_1
table_2

cost_1 = table_1[[2]]*20 + table_1[[3]]*10
cost_2 = table_2[[2]]*20 + table_2[[3]]*10
c(cost_1, cost_2)

#standardized  deviance residuals
dev_res=rstandard(step_fit_new) 
plot(dev_res~step_fit_new$fitted)
index=seq(1:3997)
plot(dev_res~index)
plot(step_fit_new, which=c(2))
plot(step_fit_new, which=c(4))

summary(step_fit_new)



'''
#interaction
attach(data_test)

ggplot() +
  aes(x = data_train$state_new, color = data_train$international_plan , group = data_train$international_plan, y = data_train$churn) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('state_new:intl_plan')

ggplot() +
  aes(x = data_train$number_customer_service_calls, color = data_train$international_plan , group = data_train$international_plan, y = data_train$churn) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('intl_plan:cus_ser_call')

ggplot() +
  aes(x = data_train$voice_mail_plan, color = data_train$number_customer_service_calls , group = data_train$number_customer_service_calls, y = data_train$churn) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('vmail_plan:cus_ser_call')

summary(step_fit_new)

# Test for interaction terms 
new_fit_int=update(step_fit_new,.~.+number_customer_service_calls*international_plan, data = data_train )
library(lmtest)
lrtest(step_fit_new,new_fit_int)
plot(new_fit_int, which=c(2))

# Test for not linear terms
new_var = (data_train$total_day_minutes) ^2
new_fit_square=update(step_fit_new,.~.+new_var)
lrtest(step_fit_new,new_fit_square)
plot(new_fit_square, which=c(2))
'''




















