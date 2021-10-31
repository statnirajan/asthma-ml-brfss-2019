######### Asthma Project ####

# Load required libraries and read in the dataset
library(tidyverse)
library(mice)
getwd()
setwd("/Users/nirajanbudhathoki/Documents/STA691/BRFSS")

data1 = read.csv("datamich.csv")
names(data1)

# Select only the variables considered in the study
new_data1 <- data1 %>% 
  select(ASTHNOW, ASTHMA3, X_SEX,X_AGE_G, X_EDUCAG,X_INCOMG,X_IMPRACE, EMPLOY1,X_URBSTAT, VETERAN3, X_SMOKER3,USENOW3, SMOKE100,DIABETE4, X_BMI5CAT, CHCCOPD2,X_MICHD, MEDCOST, FLUSHOT7) %>%
  filter(ASTHMA3 %in% c(1,2))

head(new_data1,10)
attach(new_data1)
table(ASTHMA3)
table(ASTHNOW)
newdata <- new_data1[ !(new_data1$ASTHNOW %in% c(7,9)), ]

table(newdata$ASTHMA3)
table(newdata$ASTHNOW)
newdata$ASTHMA = ifelse(newdata$ASTHMA3 == 1 & newdata$ASTHNOW == 1, 1, 0)
dim(newdata)
table(newdata$ASTHMA)
table(newdata$ASTHMA) %>% prop.table()

finaldata = select(newdata,X_SEX,X_AGE_G, X_EDUCAG,X_INCOMG,X_IMPRACE, EMPLOY1,X_URBSTAT, VETERAN3, X_SMOKER3,USENOW3, SMOKE100,DIABETE4, X_BMI5CAT, CHCCOPD2, MEDCOST, FLUSHOT7, ASTHMA)
dim(finaldata)
attach(finaldata)

### covert variables from integer type to factor
col_names = names(finaldata)
finaldata[,col_names] = lapply(finaldata[,col_names],factor)

str(finaldata)
summary(finaldata)


# See percentage of missing values
p<-function(x){sum(is.na(x))/length(x)*100}
apply(finaldata,2,p)

# Use mice to impute missing data
impute<-mice(finaldata[,1:17],m=3, seed=123)
print(impute)
impute$imp$USENOW3
impute$imp$SMOKE100
asthdata <- complete(impute,2)
summary(asthdata)


# Export data with imputation for further use.
write.csv(asthdata,"asthdata.csv")

############## Read ASTHMA DATA ###############################################
asthdata <- read.csv("asthdata.csv")

# Again, covert variables from integer type to factor.

col_names = names(asthdata)
asthdata[,col_names] = lapply(asthdata[,col_names],factor)

str(asthdata)
summary(asthdata)


# Distribution of response variable
ggplot(data = asthdata)+
  geom_bar(mapping = aes(x=ASTHMA,y=stat(prop),group=1))

# Labeling of categories
asthdata$SEX <- factor(asthdata$SEX,
                    levels = c(1,2),
                    labels = c("Male", "Female"))

asthdata$AGE_G <- factor(asthdata$AGE_G,
                          levels = c(1,2,3,4,5,6),
                          labels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54","55 - 64", "65 or older"))
asthdata$EDUCAG <- factor(asthdata$EDUCAG,
                          levels = c(1,2,3),
                          labels = c("Below High School", "High School","Some college or more"))
asthdata$INCOMG <- factor(asthdata$INCOMG,
                             levels = c(1,2,3,9),
                             labels = c("Below $25000", "$25000 - $50000","$50000 or more","Don't know/Not Sure"))

asthdata$IMPRACE <- factor(asthdata$IMPRACE,
                             levels = c(1,2,3,5,6),
                             labels = c("White,Non-Hispanic", "Black,Non-Hispanic","Asian,Non-Hispanic","Hispanic","Other"))
asthdata$EMPLOY1 <- factor(asthdata$EMPLOY1,
                             levels = c(1,3,5,6,7,8),
                             labels = c("Employed", "Out of work","Homemaker","Student","Retired","Unable to work"))
asthdata$URBSTAT <- factor(asthdata$URBSTAT,
                          levels = c(1,2),
                          labels = c("Urban counties", "Rural counties"))
asthdata$VETERAN3 <- factor(asthdata$VETERAN3,
                          levels = c(1,2),
                          labels = c("Yes", "No"))
asthdata$SMOKER3 <- factor(asthdata$SMOKER3,
                          levels = c(1,2,3),
                          labels = c("Current Smoker", "Former Smoker","Never Smoked"))
asthdata$USENOW3 <- factor(asthdata$USENOW3,
                          levels = c(1,3),
                          labels = c("Yes", "No"))
asthdata$SMOKE100 <- factor(asthdata$SMOKE100,
                          levels = c(1,2),
                          labels = c("Yes", "No"))
asthdata$DIABETE4 <- factor(asthdata$DIABETE4,
                             levels = c(1,3,4),
                             labels = c("Yes","No","Pre-diabetes or borderline diabetes"))
asthdata$BMI5CAT <- factor(asthdata$BMI5CAT,
                              levels = c(1,2,3,4),
                              labels = c("Underweight", "Normal weight","Overweight", "Obese"))
asthdata$CHCCOPD2 <- factor(asthdata$CHCCOPD2,
                             levels = c(1,2),
                             labels = c("Yes", "No"))
asthdata$MEDCOST <- factor(asthdata$MEDCOST,
                             levels = c(1,2),
                             labels = c("Yes", "No"))
asthdata$FLUSHOT7 <- factor(asthdata$FLUSHOT7,
                             levels = c(1,2),
                             labels = c("Yes", "No"))
summary(asthdata)
attach(asthdata)

# Some visualizations 

# Distribution of response variable
ggplot(data = asthdata)+
  geom_bar(mapping = aes(x=ASTHMA,y=stat(prop),group=1))


ggplot(asthdata, aes(x = SEX, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Gender vs Asthma")


ggplot(data = asthdata, aes(x = AGE_G, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Age group vs Asthma")+
  coord_flip()

ggplot(asthdata, aes(x = EDUCAG, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Education vs Asthma")+
  coord_flip()

ggplot(asthdata, aes(x = INCOMG, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Income vs Asthma")+
  coord_flip()

ggplot(asthdata, aes(x = URBSTAT, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Urban/Rural Status vs Asthma")

ggplot(asthdata, aes(x = DIABETE4, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Diabetes vs Asthma")

ggplot(asthdata, aes(x = BMI5CAT, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Body Mass Index vs Asthma")+
  coord_flip()

ggplot(asthdata, aes(x = CHCCOPD2, fill =ASTHMA)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Chronic Obstructive Pulmonary Disease vs Asthma")



# Our algorithm gets biased towards the majority class and fails to map minority class ##
## Random Oversampling Examples #######
table(ASTHMA)
prop.table(table(ASTHMA))  #check class proportion

# Use ROSE package to address imbalanced data.

library(ROSE)
library(tidyverse)

set.seed(101)
train = sample(1:nrow(asthdata),nrow(asthdata)*0.7)  # samples without replacement
data.train = asthdata[train, ]
data.test = asthdata[-train, ]

table(data.train$ASTHMA) %>% prop.table()

# ROSE helps to generate data synthetically as well. This data generated using ROSE is considered to provide better estimate of original data.
data.rose <- ROSE(ASTHMA~.,data=data.train,seed = 1)$data
table(data.rose$ASTHMA) %>% prop.table()
# This generated data has size equal to the original dataset..


# Chi-square tests to see association between response and predictors
table(SEX,ASTHMA)
chisq.test(SEX,ASTHMA)
table(AGE_G,ASTHMA)
chisq.test(AGE_G,ASTHMA)
table(EDUCAG,ASTHMA)
chisq.test(EDUCAG,ASTHMA)
table(INCOMG,ASTHMA)
chisq.test(INCOMG,ASTHMA)
table(IMPRACE,ASTHMA)
chisq.test(IMPRACE,ASTHMA)
chisq.test(EMPLOY1,ASTHMA)
table(URBSTAT,ASTHMA)
chisq.test(URBSTAT,ASTHMA)  # Not significant
table(VETERAN3,ASTHMA)
chisq.test(VETERAN3,ASTHMA)
table(EMPLOY1,ASTHMA)
chisq.test(EMPLOY1,ASTHMA)
table(SMOKER3,ASTHMA)
chisq.test(SMOKER3,ASTHMA)
table(USENOW3,ASTHMA)
chisq.test(USENOW3,ASTHMA)  # Not significant
table(SMOKE100,ASTHMA)
chisq.test(SMOKE100,ASTHMA)
table(DIABETE4,ASTHMA)
chisq.test(DIABETE4,ASTHMA)
table(BMI5CAT,ASTHMA)
chisq.test(BMI5CAT,ASTHMA)
table(CHCCOPD2,ASTHMA)
chisq.test(CHCCOPD2,ASTHMA)
table(MEDCOST,ASTHMA)
chisq.test(MEDCOST,ASTHMA)
table(FLUSHOT7,ASTHMA)
chisq.test(FLUSHOT7,ASTHMA)


# Drop insignicant variables
data.train = data.rose %>% select(-URBSTAT, -USENOW3)
data.test = data.test %>% select(-URBSTAT, -USENOW3)

###### Logistic Regression #######
library(caret)
library(pROC)
fitControl <- trainControl(method = "cv", number = 10)
mod_logistic <- train(ASTHMA~., 
                      data = data.train, 
                      method = "glm", 
                      family = "binomial", 
                      trControl = fitControl)
summary(mod_logistic)

logistic.pred <- predict(mod_logistic, newdata = data.test,type ="raw")
confusionMatrix(data = logistic.pred, data.test$ASTHMA, positive = "1")

roc(data.test$ASTHMA, as.numeric(logistic.pred),plot=TRUE,legacy.axes =TRUE)
roc(data.test$ASTHMA, as.numeric(pred.test))
par(pty = "s")
imp = varImp(mod_logistic)
plot(imp)

conf.mat = table(logistic.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))

#fmeasure = 2*TP/ (2*TP + FP + FN)
(fmeasure_logistic = 2*TP/(2*TP + FP + FN))

# G-Mean = sqrt(sensitivity*specificity)
(gmean_logistic = sqrt(sensit*specif))

# Mathews Correlation Coefficient

(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))

## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))



## Partial Least Squares ###
set.seed(202)
mod_pls <- train(ASTHMA ~., 
                 data = data.train,
                 method = "pls",
                 metric = "Accuracy",
                 tuneLength = 5,
                 trControl = fitControl)

# Check CV profile
plot(mod_pls)
mod_pls$bestTune

pls.pred = predict(mod_pls, newdata= data.test)
confusionMatrix(data = pls.pred, data.test$ASTHMA, positive = "1")
roc(data.test$ASTHMA, as.numeric(pls.pred),plot=TRUE,legacy.axes =TRUE)
imp_pls = varImp(mod_pls)
plot(imp_pls)

conf.mat = table(pls.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))


##### Random forest ###############
set.seed(1)
mod_rf <- train(ASTHMA~., 
                data = data.train, 
                method = "rf",
                metric = "Accuracy",
                tunelength = 5,
                trControl = fitControl)

summary(mod_rf)
rf.pred <- predict(mod_rf, newdata = data.test)
confusionMatrix(data = rf.pred, data.test$ASTHMA, positive = "1")
roc(data.test$ASTHMA, as.numeric(rf.pred),plot=TRUE,legacy.axes =TRUE)
imp_rf = varImp(mod_rf)
plot(imp_rf)

conf.mat = table(rf.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))


####### Gradient Boosting #########
set.seed(11)
mod_gb <- train(ASTHMA~.,
                data = data.train, 
                method = "gbm",
                trControl = fitControl,
                tuneGrid = expand.grid(n.trees = c(50,100,150,300,500), interaction.depth=c(1:3),shrinkage=c(0.01,0.05,0.1),n.minobsinnode=c(200))
)

summary(mod_gb)
gbm.pred <- predict(mod_gb, newdata = data.test)

table(gbm.pred,data.test$ASTHMA)

roc(data.test$ASTHMA, as.numeric(gbm.pred),plot=TRUE,legacy.axes =TRUE,print.AUC=TRUE)


conf.mat = table(gbm.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))

###### LASSO ###########
lambda <- 10^seq(-3, 3, length = 100)
set.seed(123)
mod_lasso <- train(ASTHMA ~., 
                   data = data.train, 
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
summary(mod_lasso)
lasso.pred <- predict(mod_lasso, newdata = data.test)

table(lasso.pred, data.test$ASTHMA)
mod_lasso$results
plot(mod_lasso$finalModel)

data.frame(
  as.data.frame.matrix(coef(mod_lasso$finalModel, mod_lasso$bestTune$lambda))
) %>%
  rename(Lasso_coef = X1)

(1923+216)/3124
216/(216+134)
1923/(1923+851)
roc(data.test$ASTHMA, as.numeric(lasso.pred),plot=TRUE,legacy.axes =TRUE)
imp_lasso = varImp(mod_lasso)
plot(imp_lasso)

conf.mat = table(lasso.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))

### Elastic Net ###########
set.seed(11)
mod_enet = train(ASTHMA~.,
                 data = data.train,
                 method = "glmnet",
                 trControl = fitControl,
                 tuneLength = 5)

mod_enet
summary(mod_enet)
enet.pred <- predict(mod_enet, newdata = data.test)

table(enet.pred, data.test$ASTHMA)
(1922+214)/3124
214/(214+136)
1922/(1922+852)
roc(data.test$ASTHMA, as.numeric(enet.pred),plot=TRUE,legacy.axes =TRUE)
imp_enet = varImp(mod_enet)
plot(imp_enet)

conf.mat = table(enet.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))



####### KNN ################
set.seed(3)
mod_knn <- train(ASTHMA ~.,
                 data = data.train, 
                 method = "knn",
                 trControl=fitControl,
                 tuneLength = 10)
mod_knn
confusionMatrix(mod_knn)
plot(mod_knn)
knn.pred <- predict(mod_knn, newdata = data.test)
confusionMatrix(data = knn.pred, data.test$ASTHMA, postive = "1") #has error
table(knn.pred,data.test$ASTHMA)
(1641+201)/3124  # Accuracy
201/(201+149)    # Sensitivity
1641/(1641+1133) # Specificity

roc(data.test$ASTHMA, as.numeric(knn.pred),plot=TRUE,legacy.axes =TRUE)
imp_knn = varImp(mod_knn)
plot(imp_knn)

conf.mat = table(knn.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))


######### SVM ##################
set.seed(101)
mod_svm <- train(ASTHMA ~., 
                 data = data.train, 
                 method = "svmLinear", 
                 trControl = fitControl, 
                 tuneGrid = expand.grid(C = seq(0, 2, length = 20)))
mod_svm

plot(mod_svm) # plot model accuracy versus different values of cost
# Print the best tuning parameter C that
# maximizes model accuracy
mod_svm$bestTune

svm.pred <- predict(mod_svm, newdata = data.test)
svm.pred
confusionMatrix(svm.pred, data.test$ASTHMA, positive = "1")
roc(data.test$ASTHMA, as.numeric(svm.pred),plot=TRUE,legacy.axes =TRUE)
imp_svm = varImp(mod_svm)
plot(imp_svm)


conf.mat = table(svm.pred,data.test$ASTHMA)
TP = conf.mat[2,2]
TN = conf.mat[1,1]
FP = conf.mat[2,1]
FN = conf.mat[1,2]
(sensit = TP/(TP+FN))
(specif = TN/(TN+FP))
(fmeasure_pls = 2*TP/(2*TP + FP + FN))  # F measure
(gmean_pls = sqrt(sensit*specif))       # G measure
(TP*TN - FP*FN)/sqrt(prod.integer64((TP+FP),(TP+FN),(TN+FP),(TN+FN)))   # Matthew's Cor Coef
## Kohen's Kappa ##
ta = (TP+TN)/(TP+TN+FP+FN)
ra = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(TP+TN+FP+FN)^2
(kappa = (ta - ra)/(1 - ra))
