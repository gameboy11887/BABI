setwd("/Users/Amit_Drive/AMIT's folder/Amit Doc/PGP GLIM BABI/Course/CAPSTONE/FINAL Project/NBFC_Loan_Foreclosure/")

install.packages("readxl")
install.packages("DataExplorer")
install.packages("esquisse")
install.packages("plyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("reshape2")
install.packages("usdm")
install.packages("randomForest")
install.packages("esquisse")
install.packages("caTools")
install.packages("caret")
install.packages("lmtest")
install.packages("ROCR")
install.packages("ineq")
install.packages("gbm")

library(readxl)
library(DataExplorer)
library(reshape2)
library(esquisse)
library(plyr)
library(ggplot2)
library(corrplot)
library(usdm)
library(randomForest)
library(esquisse)
library(caTools)
library(caret)
library(lmtest)
library(ROCR)
library(ineq)
library(gbm)
library(glmnet)

#Reading Raw Data file
LF = read_excel("NBFC Loan Transaction Data.xlsx")

################ 2) EDA  ################

dim(LF)
class(LF)
str(LF)
names(LF)
summary(LF)
View(LF)

#Columns with NA's count
colSums(is.na(LF))

LF = as.data.frame(LF)
class(LF)

#Creating a Data Profile report using Data Explorer to get an intial overview of data
create_report(LF)

#Dropping the below variables as these are not significant to predict foreclosure.
#Time variables are not relevant in this problem statement as it is NOT a Time series problem
# AGREEMENTID, CUSTOMERID, SCHEMEID
# AUTHORIZATIONDATE, INTEREST_START_DATE, LAST_RECEIPT_DATE 
# CITY - Not a domain specific variable

LF = subset(LF, select = -c(AGREEMENTID,CUSTOMERID,SCHEMEID,AUTHORIZATIONDATE,
                            INTEREST_START_DATE,LAST_RECEIPT_DATE,CITY))
dim(LF)

# Dropping Variables with Missing values > 30% as recommended by Data Explorer visualization
#NPA_IN_LAST_MONTH
#NPA_IN_CURRENT_MONTH

LF = subset(LF, select = -c(NPA_IN_LAST_MONTH,NPA_IN_CURRENT_MONTH))
dim(LF)
 
# Variable Transformations: Converting char/num data type variables into factor

LF$PRODUCT = as.factor(LF$PRODUCT)
LF$FORECLOSURE = as.factor(LF$FORECLOSURE)

str(LF)
summary(LF)


######## EDA on variables BEFORE treatment ##########

LF_num = subset(LF, select = -c(PRODUCT,FORECLOSURE))
dim(LF_num)
str(LF_num)

# UNIVARIATE Analysis

# Histograms

par(mfrow=c(3, 3))
colnames <- dimnames(LF_num)[[2]]
for (i in names(LF_num)) {
  hist(LF_num[,i], main  = colnames[i] ,xlab = i, ylab = "Frequency", probability=TRUE, col="violet", border="white")
}

#Boxplots

#To plot multiple Boxplots in a single view

set.seed(4)
#par(mar=c(1,1,1,1))
par(mfrow=c(3, 3))
for(i in names(LF_num)){
  boxplot(LF_num[[i]] ,main = i, col="yellow",label=TRUE,horizontal = TRUE)
}


# CATEGORICAL Variables

ggplot(LF) +
  aes(x = PRODUCT, fill = PRODUCT) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

ggplot(LF) +
  aes(x = FORECLOSURE, fill = FORECLOSURE) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

#BI-VARIATE Analysis

#Categorical Vs Categorical(Dependent variable-FORECLOSURE)

ggplot(LF) +
  aes(x = PRODUCT, fill = FORECLOSURE) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

#### EDA Bi-variate Analysis on untreated data set ###

LF_v4 = cbind(LF_num,LF$FORECLOSURE)
names(LF_v4)
names(LF_v4)[43] <- c("FORECLOSURE")


par(mfrow=c(4, 4))
LF_v4.m <- melt(LF_v4, id.var = "FORECLOSURE")
p <- ggplot(data = LF_v4.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=FORECLOSURE))
p + facet_wrap( ~ variable, scales="free")


########### 3) Data Cleaning and Pre-processing #############

# Outlier Treatment - Outlier Treatment

#To plot multiple Boxplots in a single view

set.seed(4)
par(mfrow=c(3, 3))
for(i in names(LF_num)){
  boxplot(LF_num[[i]] ,main=i,LFl="blue",label=TRUE,horizontal = TRUE)
}

#Count of Outliers in each of the numerical variables in the data set
for(i in names(LF_num)){
  
  cnt = NROW(boxplot(LF_num[[i]] ,main=i,col="pink",label=TRUE,horizontal = TRUE)$out)
  print(c(i, ":", cnt))
              
}


# From the boxplots it is clear except "NET_LTV" all other variables have outliers in the data set. Therefore,we treat 
# them as per below


#Treating outliers by replacing with Upper Whisker/Lower Whisker values

set.seed(3)
for(i in names(LF_num)){
  x <- LF_num[[i]]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  whisker <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - whisker)] <- (qnt[1] - whisker)
  x[x > (qnt[2] + whisker)] <- (qnt[2] + whisker)
  LF_num[[i]] = x
  
}

#Plotting the boxplots after Outlier Treatment

set.seed(4)
par(mfrow=c(4, 4))
for(i in names(LF_num)){
  boxplot(LF_num[[i]] ,main=i,col="blue",label=TRUE,horizontal = TRUE)
}

summary(LF_num)

# Missing Value Treatment

#Impute NAs with Median

LF_num[] <- lapply(LF_num, function(x) { 
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
})


summary(LF_num)
dim(LF_num)
names(LF_num)
class(LF_num)
View(LF_num)

# Dropping the numerical variables with same values for all observations as identified from the data summary
# These variables are not relevant predictors for foreclosure

LF_num = subset(LF_num,select = -c(DIFF_AUTH_INT_DATE,DPD,DUEDAY,EMI_OS_AMOUNT,LATEST_TRANSACTION_MONTH,PRE_EMI_OS_AMOUNT))

dim(LF_num)
summary(LF_num)


#New Variable Creations

LF_num$out_prin_bal_Ten = LF_num$OUTSTANDING_PRINCIPAL/LF_num$BALANCE_TENURE
LF_num$net_rec_max_emi_amt = LF_num$NET_RECEIVABLE/LF_num$MAX_EMI_AMOUNT
LF_num$last_Rec_net_rec = LF_num$LAST_RECEIPT_AMOUNT/LF_num$NET_RECEIVABLE
names(LF_num)
dim(LF_num)

#Summary of New Variable Ratios
summary(LF_num[,37:39])
par(mfrow=c(1, 1))
boxplot(LF_num[,37:39],horizontal = T)


#New Variable Treatments. Replacing 'Inf' with NAs
is.na(LF_num$last_Rec_net_rec) = sapply(LF_num$last_Rec_net_rec,is.infinite)
summary(LF_num$last_Rec_net_rec)
boxplot(LF_num$last_Rec_net_rec, main = "Last_Received_Amt/Net_Receivables",horizontal = T)

#Since this Ratio variable has >50% of the values as NA's, its not worth imputing these NA's with median/mean
# Hence we drop this ratio.

LF_num = subset(LF_num,select = -c(last_Rec_net_rec))

names(LF_num)[37:38] <- c( "OUT_PRIN_by_BAL_TEN", "NET_REC_by_MAX_EMI_AMT")

# Treating Outliers in the New Ratios

LF_num_v1 = subset(LF_num, select = c(37:38))
summary(LF_num_v1)

# Q1) Treating outliers in the NEW created ratio variables

set.seed(4)
par(mfrow=c(2, 2))
for(i in names(LF_num_v1)){
  boxplot(LF_num_v1[[i]] ,main=i,col="blue",label=TRUE,horizontal = TRUE)
}


set.seed(3)
for(i in names(LF_num_v1)){
  x <- LF_num_v1[[i]]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  whisker <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - whisker)] <- (qnt[1] - whisker)
  x[x > (qnt[2] + whisker)] <- (qnt[2] + whisker)
  LF_num_v1[[i]] = x
  
}

#Boxplots of newly created variables after Outlier treatment
set.seed(4)
par(mfrow=c(2, 2))
for(i in names(LF_num_v1)){
  boxplot(LF_num_v1[[i]] ,main=i,col="green",label=TRUE,horizontal = TRUE)
}

summary(LF_num_v1)

#There are NO missing values in the new Ratios.

#Removing the untreated newly created columns from 'LF_num' data frame

LF_num = subset(LF_num, select = -c(37:38))
LF_num = cbind(LF_num,LF_num_v1)
names(LF_num)
dim(LF_num)
summary(LF_num)

# Removing Multicollinearity
vifcor(LF_num)
summary(LF_num)
View(LF_num)

#Dropping variable which are highly multicollinearity as suggested by output of vifcor function

LF_num = subset(LF_num,select = -c(PRE_EMI_RECEIVED_AMT, NET_DISBURSED_AMT, EMI_RECEIVED_AMT, MONTHOPENING, COMPLETED_TENURE, CURRENT_INTEREST_RATE,
                                   EMI_DUEAMT, BALANCE_TENURE, CURRENT_INTEREST_RATE_MIN, OUTSTANDING_PRINCIPAL, ORIGNAL_INTEREST_RATE, NET_RECEIVABLE,
                                   MAX_EMI_AMOUNT,DIFF_CURRENT_INTEREST_RATE_MAX_MIN))
dim(LF_num)
par(mfrow=c(1, 1))
corrplot(cor(LF_num),method = "number",type = c("lower"))


#Cbinding Treated numerical variables and other factor variables including dependent variable "Foreclosure"

LF_v1 = cbind(LF_num,LF$PRODUCT,LF$FORECLOSURE)
names(LF_v1)[25:26] <- c( "PRODUCT", "FORECLOSURE")

View(LF_v1)
dim(LF_v1)
names(LF_v1)
str(LF_v1)
summary(LF_v1)

#Run a Random Forest to identify the Top 10 key predictor variables

seed=1000
set.seed(seed)
rndFor = randomForest(LF_v1$FORECLOSURE ~ ., data = LF_v1, 
                      ntree=501, mtry = 3, nodesize = 10,
                      importance=TRUE)

print(rndFor)
importance(rndFor)
dim(LF_v1)

#Select the Top 10(good mix of continous + discrete) important predictor variables based on RF variable importance
#Varaiables where the meandec in Accuracy/gini is high are the most important

LF_v2 = subset(LF_v1, select = c(CURRENT_INTEREST_RATE_MAX, CURRENT_INTEREST_RATE_CHANGES, DIFF_ORIGINAL_CURRENT_INTEREST_RATE,
                                  DIFF_ORIGINAL_CURRENT_TENOR, LAST_RECEIPT_AMOUNT, MIN_EMI_AMOUNT, PAID_INTEREST, PAID_PRINCIPAL,
                                  MOB, OUT_PRIN_by_BAL_TEN, FOIR, NET_LTV, PRODUCT, FORECLOSURE))

dim(LF_v2)
names(LF_v2)
str(LF_v2)
summary(LF_v2)

##### EDA & Visualization on FINAL important variables ###

# UNIVARIATE Analysis

num <- sapply(LF_v2, is.numeric)
LF_num <- LF_v2[,num]
dim(LF_num)
str(LF_num)

par(mfrow=c(4, 4))
colnames <- dimnames(LF_num)[[2]]
for (i in names(LF_num)) {
  hist(LF_num[,i], main  =colnames[i], xlab=i ,ylab = "Frequency", probability=TRUE, col="red", border="white")
}

#Boxplots

#To plot multiple Boxplots in a single view

set.seed(4)
par(mfrow=c(4, 4))
for(i in names(LF_num)){
  boxplot(LF_num[[i]] ,main=i,col="blue",label=TRUE,horizontal = TRUE)
}

# CATEGORICAL Variables

ggplot(LF_v2) +
  aes(x = PRODUCT, fill = PRODUCT) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

ggplot(LF_v2) +
  aes(x = FORECLOSURE, fill = FORECLOSURE) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

#BI-VARIATE Analysis

#Categorical Vs Categorical(Dependent variable-FORECLOSURE)

ggplot(LF_v2) +
  aes(x = PRODUCT, fill = FORECLOSURE) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

#Numerical Vs Categorical (Dependent variable-FORECLOSURE)

names(LF_v2)

#Dropping the factor column "PRODUCT"
LF_v3 = drop_columns(LF_v2,13)
LF_v3.m <- melt(LF_v3, id.var = "FORECLOSURE")
p <- ggplot(data = LF_v3.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=FORECLOSURE))
p + facet_wrap( ~ variable, scales="free")

# Corrplot of the most significant variables
par(mfrow=c(1, 1))
corrplot(cor(LF_num),method = "number",type = c("lower"))

# Scaling the Data set

LF_num = scale(LF_num, center = T, scale = T)
dim(LF_num)
summary(LF_num)
class(LF_num)
LF_num  = as.data.frame(LF_num)

#Cbinding with categrical and dependent variable "Foreclosure"
LF_v2 = cbind(LF_num,LF_v2$PRODUCT,LF_v2$FORECLOSURE)
names(LF_v2)[13:14] <- c("PRODUCT", "FORECLOSURE")


# Treating unbalanced Data

dim(LF_v2)
str(LF_v2)
summary(LF_v2)
#Calculating the interest class % in the dependent varaible "FORECLOSURE" in the Data Set
#Current event rate of Target variable interest class '1' on the WHOLE data set
NROW(LF_v2$FORECLOSURE[LF_v2$FORECLOSURE == "1"])
Event_rate = (NROW(LF_v2$FORECLOSURE[LF_v2$FORECLOSURE == "1"])/NROW(LF_v2$FORECLOSURE)) * 100
Event_rate

#Objective is to balance the data set by upsampling the interest class "FORECLOSURE = 1"

# Splitting the original treated Data Set in a 90:10 split to reduce the number of data records used to build & run the 
# models for better and faster performance.

set.seed(100) 
spl = sample.split(LF_v2, SplitRatio = 0.9)
LF_Train = subset(LF_v2, spl == T)
dim(LF_Train)
LF_Test = subset(LF_v2, spl == F)
dim(LF_Test)
summary(LF_Train)

table(LF_Train$FORECLOSURE)

#Current event rate of Target variable interest class "FORECLOUSRE" (Target = '1') on the LF_Train data set
NROW(LF_Train$FORECLOSURE[LF_Train$FORECLOSURE == "1"])
Event_rate_LF_Train = (NROW(LF_Train$FORECLOSURE[LF_Train$FORECLOSURE == "1"])/NROW(LF_Train$FORECLOSURE)) * 100
Event_rate_LF_Train

#Current event rate of Target variable interest class "FORECLOUSRE" (Target = '1') on the LF_Test data set
NROW(LF_Test$FORECLOSURE[LF_Test$FORECLOSURE == "1"])
Event_rate_LF_Test = (NROW(LF_Test$FORECLOSURE[LF_Test$FORECLOSURE == "1"])/NROW(LF_Test$FORECLOSURE)) * 100
Event_rate_LF_Test

#Assign the 10% Test data for further split into Train & Test for model building

dim(LF_Test)

# Creating 3 Dummy variables for Categorical variable "Product" with 4 levels

results <- fastDummies::dummy_cols(LF_Test$PRODUCT, remove_first_dummy = TRUE)
head(results)
str(results)
summary(LF_Test)
LF_Test = cbind(LF_Test, results)
summary(LF_Test)
LF_Test = drop_columns(LF_Test,15)
LF_Test$.data_LAP = as.factor(LF_Test$.data_LAP)
LF_Test$.data_STHL = as.factor(LF_Test$.data_STHL)
LF_Test$.data_STLAP = as.factor(LF_Test$.data_STLAP)
names(LF_Test)[15:17] <- c("Prod_LAP","Prod_STHL","Prod_STLAP")
LF_Test = drop_columns(LF_Test,13)
dim(LF_Test)
LF_Test = LF_Test [, c(1:12,14:16,13)]
dim(LF_Test)
str(LF_Test)
View(LF_Test)


# Splitting 10% of Test data of the original Data Set
set.seed(200) 
spl = sample.split(LF_Test, SplitRatio = 0.7)
LF_Train_v1 = subset(LF_Test, spl == T)
dim(LF_Train_v1)
LF_Test_v1 = subset(LF_Test, spl == F)
dim(LF_Test_v1)
summary(LF_Train_v1)
summary(LF_Test_v1)
table(LF_Train_v1$FORECLOSURE)
table(LF_Test_v1$FORECLOSURE)

#Current event rate of Target variable interest class "FORECLOUSRE" (Target = '1') on the LF_Train_v1 data set
NROW(LF_Train_v1$FORECLOSURE[LF_Train_v1$FORECLOSURE == "1"])
Event_rate_LF_Train_v1 = (NROW(LF_Train_v1$FORECLOSURE[LF_Train_v1$FORECLOSURE == "1"])/NROW(LF_Train_v1$FORECLOSURE)) * 100
Event_rate_LF_Train_v1


####### 4), 5) MODEL BUILDING & VALIDATION ##########

predictors<-c('CURRENT_INTEREST_RATE_MAX','CURRENT_INTEREST_RATE_CHANGES',
              'DIFF_ORIGINAL_CURRENT_INTEREST_RATE','DIFF_ORIGINAL_CURRENT_TENOR',
              'LAST_RECEIPT_AMOUNT','MIN_EMI_AMOUNT',
              'PAID_INTEREST','PAID_PRINCIPAL','MOB',
              'OUT_PRIN_by_BAL_TEN','FOIR','NET_LTV',
              'Prod_LAP','Prod_STHL','Prod_STLAP')

outcome <- 'FORECLOSURE'

#1)################# Log Regression ##################

# Build the model using "caret" pkg
set.seed(300)
control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3, sampling = 'up')
model_glm <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                  method = 'glm', trControl = control, tuneLength = 3)
model_glm
summary(model_glm)
varImp(model_glm)

#Analysing the coefficients of key predictor variables.

exp(1.43529)
exp(-1.37557)
exp(1.08841)
exp(0.39168)
exp(0.56609)
exp(0.68810)
exp(-0.72628)
exp(0.36584)
exp(-0.19371)
exp(-3.70823)

#Predicting using Log Reg model on Test Data
LF_Test_v1$pred_glm_outcome <- predict(object = model_glm, newdata = LF_Test_v1[, predictors])
LF_Test_v1$pred_glm_outcome_prob <- predict(object = model_glm, newdata = LF_Test_v1[, predictors], type = c("prob"))


#Checking the accuracy of the Log Reg model on Test Data set
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$pred_glm_outcome)

#Plotting ROC curve
ROCRpred = prediction(LF_Test_v1$pred_glm_outcome_prob$`1`,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_test = performance(ROCRpred, "tpr","fpr")

plot(perf_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:Log Reg Model TEST SET") 

KS_test_glm = max(attr(perf_test, 'y.values')[[1]]-attr(perf_test, 'x.values')[[1]])
auc_test_glm = performance(ROCRpred,"auc") 
auc_test_glm = as.numeric(auc_test_glm@y.values)
gini_test_glm = ineq(LF_Test_v1$pred_glm_outcome_prob$`1`, type="Gini")

#Printing KS, AUC & gini for CART test Data Set

KS_test_glm
auc_test_glm
gini_test_glm

################# Random Forest ##################

model_rf <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                  method = 'rf', trControl = control, tuneLength = 3)
model_rf
summary(model_rf)
varImp(model_rf)

#Predicting using Random Forest model on Test Data
LF_Test_v1$pred_rf_outcome <- predict(object = model_rf, newdata = LF_Test_v1[, predictors])
LF_Test_v1$pred_rf_outcome_prob <- predict(object = model_rf, newdata = LF_Test_v1[, predictors], type = c("prob"))

head(LF_Test_v1$pred_rf_outcome_prob)

#Checking the accuracy of the Random Forest model on Test Data
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$pred_rf_outcome)

#Plotting ROC curve
ROCRpred = prediction(LF_Test_v1$pred_rf_outcome_prob$`1`,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_test = performance(ROCRpred, "tpr","fpr")

plot(perf_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:RF Model TEST SET") 

KS_test_rf = max(attr(perf_test, 'y.values')[[1]]-attr(perf_test, 'x.values')[[1]])
auc_test_rf = performance(ROCRpred,"auc") 
auc_test_rf = as.numeric(auc_test_rf@y.values)
gini_test_rf = ineq(LF_Test_v1$pred_rf_outcome_prob$`1`, type="Gini")

#Printing KS, AUC & gini for Random Forect on TEST Data Set

KS_test_rf
auc_test_rf
gini_test_rf

################# KNN ##################
model_knn <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                  method = 'knn', trControl = control, tuneLength = 3)
model_knn
summary(model_knn)
varImp(model_knn)

#Predicting using Random Forest model on Test Data
LF_Test_v1$pred_knn_outcome <- predict(object = model_knn, newdata = LF_Test_v1[, predictors])
LF_Test_v1$pred_knn_outcome_prob <- predict(object = model_knn, newdata = LF_Test_v1[, predictors], type = c("prob"))


#Checking the accuracy of the Random Forest model on Test Data
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$pred_knn_outcome)

#Plotting ROC curve for KNN Test Data
ROCRpred = prediction(LF_Test_v1$pred_knn_outcome_prob$`1`,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_knn_test = performance(ROCRpred, "tpr","fpr")

plot(perf_knn_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:KNN Model TEST SET") 

KS_test_knn = max(attr(perf_knn_test, 'y.values')[[1]]-attr(perf_knn_test, 'x.values')[[1]])
auc_test_knn = performance(ROCRpred,"auc") 
auc_test_knn = as.numeric(auc_test_knn@y.values)
gini_test_knn = ineq(LF_Test_v1$pred_knn_outcome_prob$`1`, type="Gini")

#Printing KS, AUC & gini for KNN on TEST Data Set

KS_test_knn
auc_test_knn
gini_test_knn

############# ENSEMBLE MODELLING - For Performance Improvement ################

#1) Averaging Ensemble Model

#Predicting the probabilities on Test Set
LF_Test_v1$pred_glm_prob <- predict(object = model_glm, LF_Test_v1[, predictors], type = 'prob')
LF_Test_v1$pred_rf_prob <- predict(object = model_rf, LF_Test_v1[, predictors], type = 'prob')
LF_Test_v1$pred_knn_prob <- predict(object = model_knn, LF_Test_v1[, predictors], type = 'prob')

#Taking average of predictions
LF_Test_v1$pred_avg <- (LF_Test_v1$pred_glm_prob$'1' + LF_Test_v1$pred_rf_prob$'1' + LF_Test_v1$pred_knn_prob$'1') / 3

plot(LF_Test_v1$FORECLOSURE,LF_Test_v1$pred_avg, main = "Probabilities of Avg Ensemble model", xlab = "Foreclosure", ylab ="Prob of Foreclosure '1'")

#Splitting into binary classes at 0.8
LF_Test_v1$pred_avg_outcome <- as.factor(ifelse(LF_Test_v1$pred_avg > 0.8, '1', '0'))

#Checking the accuracy of the Averaged Ensemble Model on Test Data
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$pred_avg_outcome)

#Plotting ROC curve for KNN Test Data
ROCRpred = prediction(LF_Test_v1$pred_avg,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_avg_ensbl_test = performance(ROCRpred, "tpr","fpr")

plot(perf_avg_ensbl_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:Avg Ensemble Model TEST SET") 

KS_test_avg_ensbl = max(attr(perf_avg_ensbl_test, 'y.values')[[1]]-attr(perf_avg_ensbl_test, 'x.values')[[1]])
auc_test_avg_ensbl = performance(ROCRpred,"auc") 
auc_test_avg_ensbl = as.numeric(auc_test_avg_ensbl@y.values)
gini_test_avg_ensbl = ineq(LF_Test_v1$pred_avg, type="Gini")

#Printing KS, AUC & gini for KNN on TEST Data Set

KS_test_avg_ensbl
auc_test_avg_ensbl
gini_test_avg_ensbl

#2) Weighted Average Ensemble Model

#Predicting the probabilities on Test Set
LF_Test_v1$pred_glm_prob <- predict(object = model_glm, LF_Test_v1[, predictors], type = 'prob')
LF_Test_v1$pred_rf_prob <- predict(object = model_rf, LF_Test_v1[, predictors], type = 'prob')
LF_Test_v1$pred_knn_prob <- predict(object = model_knn, LF_Test_v1[, predictors], type = 'prob')

#Taking average of predictions
LF_Test_v1$pred_wt_avg <- (0.20*LF_Test_v1$pred_glm_prob$'1' + 0.5*LF_Test_v1$pred_rf_prob$'1' + 0.30*LF_Test_v1$pred_knn_prob$'1') 

plot(LF_Test_v1$FORECLOSURE,LF_Test_v1$pred_wt_avg, main = "Probabilities of Wt Avg Ensemble model", xlab = "Foreclosure", ylab ="Prob of Foreclosure '1'")

#Splitting into binary classes at 0.7
LF_Test_v1$pred_wt_avg_outcome <- as.factor(ifelse(LF_Test_v1$pred_wt_avg > 0.7, '1', '0'))

#Checking the accuracy of the Averaged Ensemble Model on Test Data
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$pred_wt_avg_outcome)

#Plotting ROC curve for KNN Test Data
ROCRpred = prediction(LF_Test_v1$pred_wt_avg,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_wt_avg_ensbl_test = performance(ROCRpred, "tpr","fpr")

plot(perf_avg_ensbl_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:Weighted Avg Ensemble Model on TEST SET") 

KS_test_wt_avg_ensbl = max(attr(perf_wt_avg_ensbl_test, 'y.values')[[1]]-attr(perf_wt_avg_ensbl_test, 'x.values')[[1]])
auc_test_wt_avg_ensbl = performance(ROCRpred,"auc") 
auc_test_wt_avg_ensbl = as.numeric(auc_test_wt_avg_ensbl@y.values)
gini_test_wt_avg_ensbl = ineq(LF_Test_v1$pred_wt_avg, type="Gini")

#Printing KS, AUC & gini for KNN on TEST Data Set

KS_test_wt_avg_ensbl
auc_test_wt_avg_ensbl
gini_test_wt_avg_ensbl

#3) Stacked Ensemble Model 1

#Building a Stacked Ensemble model using caret pkg using 3 base layer models - Log Reg, RF & KNN

# Step 1: Train the individual base layer models on training data

set.seed(300)
control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3, sampling = 'up')
model_glm <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                   method = 'glm', trControl = control, tuneLength = 3)

model_rf <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                   method = 'rf', trControl = control, tuneLength = 3)

model_knn <- train(LF_Train_v1[, predictors], LF_Train_v1[, outcome], 
                  method = 'knn', trControl = control, tuneLength = 3)


# Step 2: Predict using each base layer model for training data and test data

#Predicting the out of fold prediction probabilities for each Base layer on TRAIN data

LF_Train_v1$OOF_train_pred_glm <- predict(object = model_glm, LF_Train_v1[, predictors], type = 'prob')$'1'
LF_Train_v1$OOF_train_pred_rf <- predict(object = model_rf, LF_Train_v1[, predictors], type = 'prob')$'1'
LF_Train_v1$OOF_train_pred_knn <- predict(object = model_knn, LF_Train_v1[, predictors], type = 'prob')$'1'

str(LF_Train_v1)
View(LF_Train_v1)

#Predicting the out of fold prediction probabilities for each Base layer on TEST data

LF_Test_v1$OOF_test_pred_glm <- predict(object = model_glm, LF_Test_v1[, predictors], type = 'prob')$'1'
LF_Test_v1$OOF_test_pred_rf <- predict(object = model_rf, LF_Test_v1[, predictors], type = 'prob')$'1'
LF_Test_v1$OOF_test_pred_knn <- predict(object = model_knn, LF_Test_v1[, predictors], type = 'prob')$'1'
 
#Step 3: Now train the Top layer (GBM)  model again on the predictions of the bottom layer models that has been made on the training data
 
#Train Predictors for top layer model
train_predictors_top = c(LF_Train_v1$OOF_train_pred_glm,LF_Train_v1$OOF_train_pred_rf,LF_Train_v1$OOF_train_pred_knn)

# Check for correlation among the predictors of Top layer

#Cbinding the train predictions of each base layer into a data frame
train_predictors_top_new = cbind(LF_Train_v1$OOF_train_pred_glm,LF_Train_v1$OOF_train_pred_rf,LF_Train_v1$OOF_train_pred_knn)
head(train_predictors_top_new) 

corrplot(cor(train_predictors_top_new), method = "number",type = c("lower")) 

#Above corrplot shows all 3 preditor probabilities of each base layer are not correlated with max 
# value of 0.76 (between 3 & 2)

model_gbm <- train(LF_Train_v1[, train_predictors_top], LF_Train_v1[, outcome],
                   method = 'gbm', trControl = control, tuneLength = 3)


#Step 4: Predict using the top layer model with the predictions of bottom layer models that has been made for testing data

# Test Predictors for top layer model
test_predictors_top = c(LF_Test_v1$OOF_test_pred_glm,LF_Test_v1$OOF_test_pred_rf,LF_Test_v1$OOF_test_pred_knn)

str(LF_Test_v1)
View(LF_Test_v1)

#predict using GBM as top layer model
LF_Test_v1$gbm_stack_prediction <- predict(model_gbm, LF_Test_v1[, test_predictors_top])
LF_Test_v1$gbm_stack_prediction_prob <- predict(model_gbm, LF_Test_v1[, test_predictors_top], type = 'prob')$'1'

#Step 5: Finally, check the accuracy for the predictions of bottom layer models that has been made for testing data

#Accuracy of GBM top layer model
confusionMatrix(LF_Test_v1$FORECLOSURE, LF_Test_v1$gbm_stack_prediction)

#Plotting ROC curve for Stacked model on Test Data
ROCRpred = prediction(LF_Test_v1$gbm_stack_prediction_prob,LF_Test_v1$FORECLOSURE)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf_gbm_ensbl_test = performance(ROCRpred, "tpr","fpr")

plot(perf_gbm_ensbl_test, colorize =T, print.cutoffs.at= seq(0, 1, .1), text.adj = c(-.2, 1.7), main= "ROC Curve:GBM Ensemble Model TEST SET") 

KS_test_gbm_ensbl = max(attr(perf_gbm_ensbl_test, 'y.values')[[1]]-attr(perf_gbm_ensbl_test, 'x.values')[[1]])
auc_test_gbm_ensbl = performance(ROCRpred,"auc") 
auc_test_gbm_ensbl = as.numeric(auc_test_gbm_ensbl@y.values)
gini_test_gbm_ensbl = ineq(LF_Test_v1$gbm_stack_prediction_prob, type="Gini")

#Printing KS, AUC & gini for Stacked model on TEST Data Set

KS_test_gbm_ensbl
auc_test_gbm_ensbl
gini_test_gbm_ensbl

################################# END ####################################


