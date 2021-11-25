rm(list = ls())
setwd("C:/Users/npadm/Documents/Rprogram")

#--------------------------------------------LOAD DATA STARTS ---------------------------------
#install.packages('readxl')
library('readxl')
patientsData.df <- read_excel("PatientData_Original.xlsx")
#--------------------------------------------LOAD DATA ENDS------------------------------------
library(data.table)
DT <- data.table(patientsData.df)

#------------ SELECT NECESSARY COLUMNS ------------
cols <- c(15:230)

# Fill the NA values with mean of other windows 
DT <- DT[, (cols) := lapply(.SD, function(x) nafill(x, type = "const"
                                                    , fill = mean(x, na.rm = TRUE)))
         , by = PATIENT_VISIT_IDENTIFIER
         , .SDcols = cols][]

#Convert data table back to data frame
patientsData_New.df <- data.frame(DT)
patientsData_New.df <- subset(patientsData_New.df, patientsData.df$WINDOW == '0') # Select only window 12 data
patientsData_New.df <- na.omit(patientsData_New.df) # remove na values

# #
# patientsData.df <- patientsData.df[-c(3,5)] # remove string columns AGE_PERCENTIL / WINDOW

l <- length(unique(patientsData_New.df$PATIENT_VISIT_IDENTIFIER))
print(paste("TOTAL NO OF PATIENTS AFTER CLEANUP",l))

# Check how many in this dataset is admitted to ICU
t <- sum(patientsData_New.df$ICU_ADMITTED_OR_NOT)
print(paste("TOTAL NO OF PATIENTS ADMITTED TO ICU",t))

# DELETE PATIENT IDENTIFIER and WINDOW COLUMN
patientsData_New.df <- patientsData_New.df[-c(1,5,14)]

#install.packages("tidyverse")
library(tidyverse)
patientsData_New.df$AGE_PERCENTIL <- ifelse(patientsData_New.df$AGE_PERCENTIL != 'Above 90th',
                                            str_sub(patientsData_New.df$AGE_PERCENTIL,1,2),
                                            str_sub(patientsData_New.df$AGE_PERCENTIL,7,8))

# CONVERT AGE PERCENTIL TO NUMERIC
cols.num <- c(2)
patientsData_New.df[cols.num] <- lapply(patientsData_New.df[cols.num],as.numeric)
#sapply(patientsData_New.df, class)

# ------------------------------------- DATA EXPLORATION STARTS ------------------------------------


# BAR PLOT AGE_PERCENTIL VS ICU ADMISSIONS
barplot(aggregate(patientsData_New.df$ICU_ADMITTED_OR_NOT == 1, by = list(patientsData_New.df$AGE_PERCENTIL),
                  mean, rm.na = T)[,2], xlab = "AGE_PERCENTIL", ylab = "ICU ADMISSIONS",
        names.arg = c("10","20","30","40","50","60","70","80","90","Above 90th"), col = "orange")

# BAR PLOT GENDER VS ICU ADMISSIONS
barplot(aggregate(patientsData_New.df$ICU_ADMITTED_OR_NOT == 1, by = list(patientsData_New.df$GENDER),
                  mean, rm.na = T)[,2], xlab = "GENDER", ylab = "ICU ADMISSIONS",
        names.arg = c("Gender 0","Gender 1"), col = "light blue")

# BARPLOT AGE ABOVE 65 vs ICU ADMITS

barplot(aggregate(patientsData_New.df$ICU_ADMITTED_OR_NOT == 1, by = list(patientsData_New.df$AGE_ABOVE65),
                  mean, rm.na = T)[,2], xlab = "AGE( ABOVE or BELOW 65 )", ylab = "ICU ADMISSIONS",
        names.arg = c("AGE_BELOW 65","AGE_ABOVE65"), col = "pink")


#----------------------------------------- PARTITION DATA STARTS --------------------------------
train.index <- sample(c(1:dim(patientsData_New.df)[1]), dim(patientsData_New.df)[1]*0.6)
train.df <- patientsData_New.df[train.index, ]
valid.df <- patientsData_New.df[-train.index, ]
#----------------------------------------- PARTITION DATA ENDS --------------------------------



#---------------------------------- COMPUTE CORRELATION ( NEVER MIND RUNNING THIS AGAIN - WILL TAKE 3 MINS) -------------------------------------
icu_data <- patientsData_New.df[c(-2)]
res <- cor(icu_data)

# install.packages("corrplot")
# 
# library(corrplot)
# corrplot(res, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)

# install.packages("PerformanceAnalytics")
# 
# library("PerformanceAnalytics")
# chart.Correlation(icu_data, histogram=TRUE, pch=19)

#---------------------------------- COMPUTE CORRELATION ENDS -------------------------------------

# #--------------------------------- TRYING PCA --------------------------
# 
# pcs <- prcomp(patientsData_New.df)
# summary(pcs)
# pcs$rot[,1:5]
# scores <- pcs$x
# head(scores, 5)
# # #--------------------------------- TRYING PCA--------------------------
# install.packages("FactoMineR")
# require(FactoMineR) 
# 
# #read the tab file using the read table function.
# pca <- PCA(patientsData_New.df, scale.unit=TRUE, ncp=5, graph=T)
# #scale all the features,  ncp: number of dimensions kept in the results (by default 5)
# dimdesc(pca)
# #This line of code 

#---------------------------------- LOGISTIC REGRESSION STARTS ----------------------------------
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(ICU_ADMITTED_OR_NOT ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

# install.packages("jtools")    # jtools helps present regression results
library(jtools)
summ(logit.reg, digits=5)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")


# Map into TP, FP, TN, FN
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.1, "ICU ADMISSION REQD", "ICU NOT REQD")),
                as.factor(ifelse(valid.df$ICU_ADMITTED_OR_NOT==1,"ICU ADMISSION REQD", "ICU NOT REQD")))

#-------------------------------------- VARIABLE SELECTION STARTS -------------------------------

# Variable Selection
# stepwise
null <- glm(ICU_ADMITTED_OR_NOT ~ 1, data = train.df, family = "binomial") 
step(null, scope=list(lower=null, upper=logit.reg), direction="both")

#--------------------------------Run after selecting the best variable group----------------------------

# logit.var.sel <- glm(formula = ICU_ADMITTED_OR_NOT ~ RESPIRATORY_RATE_MAX + LACTATE_MEDIAN + 
#                        TEMPERATURE_DIFF + UREA_MEDIAN + BLOODPRESSURE_DIASTOLIC_MEDIAN + 
#                        BLOODPRESSURE_SISTOLIC_MEAN + NEUTROPHILES_MEDIAN + CALCIUM_MEDIAN + 
#                        RESPIRATORY_RATE_MIN + HEART_RATE_DIFF_REL + OXYGEN_SATURATION_MIN + 
#                        GGT_MEDIAN + RESPIRATORY_RATE_DIFF_REL + BLOODPRESSURE_DIASTOLIC_DIFF_REL + 
#                        P02_VENOUS_MEDIAN + BIC_VENOUS_MEDIAN + INR_MEDIAN
#                      , family = "binomial", 
#                      data = train.df)

logit.var.sel <- glm(formula = ICU_ADMITTED_OR_NOT ~ RESPIRATORY_RATE_MAX + LACTATE_MEDIAN + 
                       HTN + RESPIRATORY_RATE_MIN + RESPIRATORY_RATE_DIFF_REL + 
                       BLOODPRESSURE_DIASTOLIC_MEDIAN + BLOODPRESSURE_SISTOLIC_MEAN + 
                       NEUTROPHILES_MEDIAN + TTPA_MEDIAN + TEMPERATURE_MEDIAN + 
                       BE_VENOUS_MEDIAN + BIC_VENOUS_MEDIAN + HEMATOCRITE_MEDIAN + 
                       TEMPERATURE_MAX + BLOODPRESSURE_SISTOLIC_DIFF + BLOODPRESSURE_DIASTOLIC_DIFF + 
                       OXYGEN_SATURATION_MAX + BLOODPRESSURE_DIASTOLIC_DIFF_REL + 
                       BLOODPRESSURE_DIASTOLIC_MEAN + DISEASE.GROUPING.2 + CALCIUM_MEDIAN + 
                       OXYGEN_SATURATION_MEAN + ALBUMIN_MEDIAN + HEART_RATE_DIFF
                     , family = "binomial", 
                     data = train.df)

  summary(logit.var.sel)
  # use predict() with type = "response" to compute predicted probabilities. 
  logit.var.sel.pred <- predict(logit.var.sel, valid.df, type = "response")
  
  # Map into TP, FP, TN, FN
  confusionMatrix(as.factor(ifelse(logit.var.sel.pred > 0.1, "ICU ADMISSION REQD", "ICU NOT REQD")),
                  as.factor(ifelse(valid.df$ICU_ADMITTED_OR_NOT==1,"ICU ADMISSION REQD", "ICU NOT REQD")))
  
  #----------------------------------------TREE WITH ALL VARIABLES -----------------------------------

library(rpart)
# install.packages("rpart.plot")    # Installation is required for the first of use
library(rpart.plot)
default.ct <- rpart(ICU_ADMITTED_OR_NOT ~ ., data = train.df, method = "class")
summary(default.ct)
# define rpart.control() in rpart() to determine the depth of the tree.
default.ct <- rpart(ICU_ADMITTED_OR_NOT ~ ., data = train.df, 
                    control = rpart.control(maxdepth = 5), method = "class")
summary(default.ct)

default.ct.pred <- predict(default.ct,valid.df,type = "class")


# plot using prp()
prp(default.ct, type = 5, extra = 101,  clip.right.lab = FALSE, 
    box.palette = "GnYlRd", leaf.round = 5, 
    branch = .3, varlen = -10, space=0)



# Map into TP, FP, TN, FN
confusionMatrix(as.factor(default.ct.pred), 
                as.factor(valid.df$ICU_ADMITTED_OR_NOT))

# ---------------------------------- TREE AFTER VARIABLE SELECTION ------------------------------------
library(rpart)
# install.packages("rpart.plot")    # Installation is required for the first of use
library(rpart.plot)
default.varsel.ct <- rpart(ICU_ADMITTED_OR_NOT ~ RESPIRATORY_RATE_MAX + LACTATE_MEDIAN + 
                      HTN + RESPIRATORY_RATE_MIN + RESPIRATORY_RATE_DIFF_REL + 
                      BLOODPRESSURE_DIASTOLIC_MEDIAN + BLOODPRESSURE_SISTOLIC_MEAN + 
                      NEUTROPHILES_MEDIAN + TTPA_MEDIAN + TEMPERATURE_MEDIAN + 
                      BE_VENOUS_MEDIAN + BIC_VENOUS_MEDIAN + HEMATOCRITE_MEDIAN + 
                      TEMPERATURE_MAX + BLOODPRESSURE_SISTOLIC_DIFF + BLOODPRESSURE_DIASTOLIC_DIFF + 
                      OXYGEN_SATURATION_MAX + BLOODPRESSURE_DIASTOLIC_DIFF_REL + 
                      BLOODPRESSURE_DIASTOLIC_MEAN + DISEASE.GROUPING.2 + CALCIUM_MEDIAN + 
                      OXYGEN_SATURATION_MEAN + ALBUMIN_MEDIAN + HEART_RATE_DIFF, data = train.df, method = "class")
summary(default.varsel.ct)
default.varsel.ct.pred <- predict(default.varsel.ct,valid.df,type = "class")

# Map into TP, FP, TN, FN
confusionMatrix(as.factor(default.varsel.ct.pred), 
                as.factor(valid.df$ICU_ADMITTED_OR_NOT))



#----------------------------- NAIVE BAYES STARTS ( WITH ALL VARIABLES) ---------------------------------------------
library(e1071)  # For NB and SVM

icuadmits.nb <- naiveBayes(ICU_ADMITTED_OR_NOT ~ ., data = train.df)
icuadmits.nb


## predict probabilities: Training
train.nb.prob <- predict(icuadmits.nb, newdata = train.df, type = "raw")
## predict class membership
train.nb.class <- predict(icuadmits.nb, newdata = train.df)
confusionMatrix(as.factor(train.nb.class), as.factor(train.df$ICU_ADMITTED_OR_NOT))

## predict probabilities: Validations
valid.nb.prob <- predict(icuadmits.nb, newdata = valid.df, type = "raw")
## predict class membership
valid.nb.class <- predict(icuadmits.nb, newdata = valid.df)
confusionMatrix(as.factor(valid.nb.class), as.factor(valid.df$ICU_ADMITTED_OR_NOT))

#--------------------------------- NAIVE BAYES ENDS ---------------------------------------------

#----------------------------- NAIVE BAYES  ( WITH SELECTED VARIABLES) ---------------------------------------------
library(e1071)  # For NB and SVM

icuadmits.var.sel.nb <- naiveBayes(ICU_ADMITTED_OR_NOT ~ RESPIRATORY_RATE_MAX + LACTATE_MEDIAN + 
                             HTN + RESPIRATORY_RATE_MIN + RESPIRATORY_RATE_DIFF_REL + 
                             BLOODPRESSURE_DIASTOLIC_MEDIAN + BLOODPRESSURE_SISTOLIC_MEAN + 
                             NEUTROPHILES_MEDIAN + TTPA_MEDIAN + TEMPERATURE_MEDIAN + 
                             BE_VENOUS_MEDIAN + BIC_VENOUS_MEDIAN + HEMATOCRITE_MEDIAN + 
                             TEMPERATURE_MAX + BLOODPRESSURE_SISTOLIC_DIFF + BLOODPRESSURE_DIASTOLIC_DIFF + 
                             OXYGEN_SATURATION_MAX + BLOODPRESSURE_DIASTOLIC_DIFF_REL + 
                             BLOODPRESSURE_DIASTOLIC_MEAN + DISEASE.GROUPING.2 + CALCIUM_MEDIAN + 
                             OXYGEN_SATURATION_MEAN + ALBUMIN_MEDIAN + HEART_RATE_DIFF, data = train.df)
icuadmits.var.sel.nb


## predict probabilities: Validations
valid.nb.varsel.prob <- predict(icuadmits.var.sel.nb, newdata = valid.df, type = "raw")
## predict class membership
valid.nb.varsel.class <- predict(icuadmits.var.sel.nb, newdata = valid.df)
confusionMatrix(as.factor(valid.nb.varsel.class), as.factor(valid.df$ICU_ADMITTED_OR_NOT))

#--------------------------------- NAIVE BAYES ENDS ---------------------------------------------
#install.packages("mlr", type="source")
library(adabag)
library(rpart) 
library(caret)
# install.packages("klaR")
library(klaR)

# Build an Ensemble Model with Multiple Types of Models
# Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

predictors <- factor(c("RESPIRATORY_RATE_MAX" ,"LACTATE_MEDIAN",  
                         "HTN","RESPIRATORY_RATE_MIN","RESPIRATORY_RATE_DIFF_REL" ,
                         "BLOODPRESSURE_DIASTOLIC_MEDIAN", "BLOODPRESSURE_SISTOLIC_MEAN",
                         "NEUTROPHILES_MEDIAN", "TTPA_MEDIAN", "TEMPERATURE_MEDIAN", 
                         "BE_VENOUS_MEDIAN" , "BIC_VENOUS_MEDIAN" , "HEMATOCRITE_MEDIAN" , 
                         "TEMPERATURE_MAX" , "BLOODPRESSURE_SISTOLIC_DIFF" , "BLOODPRESSURE_DIASTOLIC_DIFF" , 
                         "OXYGEN_SATURATION_MAX" , "BLOODPRESSURE_DIASTOLIC_DIFF_REL" , 
                         "BLOODPRESSURE_DIASTOLIC_MEAN" , "DISEASE.GROUPING.2" , "CALCIUM_MEDIAN" , 
                         "OXYGEN_SATURATION_MEAN" , "ALBUMIN_MEDIAN" , "HEART_RATE_DIFF"))
subset(predictors, 
       is.na(
         match(predictors, names(patientsData.df))
       )
)
#outcomeName<-c("ICU_ADMITTED_OR_NOT")
#train.df$ICU_NB <- make.names(train.df$ICU)
#table(train.df$ICU_NB)
#valid.df$ICU_NB <- make.names(valid.df$ICU)
#table(valid.df$ICU_NB)
#outcomeName<-c("ICU_NB")
#Training a random forest model

model_rf<-train(train.df[,predictors],train.df$ICU_NB,method='rf',
                trControl=fitControl,tuneLength=3)
typeof(valid.df)
levels(valid.df) <- levels(train.df)
#Predicting using random forest model
valid.df$pred_rf<-predict(object = model_rf,valid.df)
valid.df$pred_rf.prob<-predict(object = model_rf,(valid.df[,predictors]),type="prob")

#Checking the accuracy of the random forest model
confusionMatrix(as.factor(valid.df$pred_rf), 
                as.factor(valid.df$ICU_NB))

#Training a Logistic regression model

model_lr<-train(train.df[,predictors],train.df$ICU_NB,method='glm',
                trControl=fitControl,tuneLength=3)
#Predicting using logistic model
valid.df$pred_lr<-predict(object = model_lr,valid.df)
valid.df$pred_lr.prob<-predict(object = model_lr,(valid.df[,predictors]),type="prob")
#Checking the accuracy of the logistic model
confusionMatrix(as.factor(valid.df$pred_lr), 
                as.factor(valid.df$ICU_NB))

#Training a Naive Bayes model

model_nb<-train(train.df[,predictors],train.df$ICU_NB,method='nb',
                trControl=fitControl,tuneLength=3)

#Predicting using Naive Bayes model
#str(valid.df[,predictors])
#valid.df$ICU_test <- factor(
#    as.character(valid.df$ICU), 
#    levels = predictors
#)

valid.df$pred_nb<-predict(object = model_nb,valid.df[,predictors])
valid.df$pred_nb.prob<-predict(object = model_nb,valid.df[,predictors],type="prob")
#Checking the accuracy of the Naive Bayes model
confusionMatrix(as.factor(valid.df$ICU_NB),as.factor(valid.df$pred_nb))

# install.packages("gains")
library(gains)
valid.df$ICU_NB.n = ifelse(valid.df$ICU_NB == "X1", 1, 0)
valid.df$pred_rf.n = ifelse(valid.df$pred_rf == "X1", 1, 0)
valid.df$pred_lr.n = ifelse(valid.df$pred_lr == "X1", 1, 0)
valid.df$pred_nb.n = ifelse(valid.df$pred_nb == "X1", 1, 0)

# Life chart: the main idea
lift.df <- valid.df$pred_rf.prob$X1
lift.df <- data.frame(lift.df, valid.df[,c("pred_rf.n","ICU_NB.n")])
colnames(lift.df) <- c("pred_rf.prob.1","pred_rf.n","ICU_NB.n")
head(lift.df)
attach(lift.df)
lift.df <- lift.df[order(-pred_rf.prob.1),]
detach(lift.df)
lift.df$ICU_NB.n.cum<-cumsum(lift.df$ICU_NB.n)

# Gains: Random Forest
gain.rf <- gains(valid.df$ICU_NB.n, valid.df$pred_rf.prob$X1, groups=10)
# Gains: Logistic Regression
gain.lr <- gains(valid.df$ICU_NB.n, valid.df$pred_lr.prob$X1, groups=10)
# Gains: Naive Bayes
gain.nb <- gains(valid.df$ICU_NB.n, valid.df$pred_nb.prob$X1, groups=10)


# Plot lift charts
plot(c(0, gain.rf$cume.pct.of.total*sum(valid.df$ICU_NB.n)) ~ c(0, gain.rf$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="red")
par(new=TRUE)
plot(c(0, gain.lr$cume.pct.of.total*sum(valid.df$ICU_NB.n)) ~ c(0, gain.lr$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="green")
par(new=TRUE)
plot(c(0, gain.nb$cume.pct.of.total*sum(valid.df$ICU_NB.n)) ~ c(0, gain.nb$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="blue")
lines(c(0,sum(valid.df$ICU_NB.n))~c(0,dim(valid.df)[1]), col="gray", lty=2)


# compute deciles and plot decile-wise chart
par(mfrow=c(1,3))
dec.rf <- gain.rf$mean.resp/mean(valid.df$ICU_NB.n)
barplot(dec.rf, names.arg = gain.rf$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Random Forest")
dec.lr <- gain.lr$mean.resp/mean(valid.df$ICU_NB.n)
barplot(dec.lr, names.arg = gain.lr$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Logistic Regression")
dec.nb <- gain.nb$mean.resp/mean(valid.df$ICU_NB.n)
barplot(dec.lr, names.arg = gain.nb$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Naive Bayes")


# ROC
# install.packages("pROC")
library(pROC)
roc.rf <- roc(valid.df$ICU_NB.n, valid.df$pred_rf.prob$X1)
roc.lr <- roc(valid.df$ICU_NB.n, valid.df$pred_lr.prob$X1)
roc.nb <- roc(valid.df$ICU_NB.n, valid.df$pred_nb.prob$X1)

plot(roc.rf,col="red")
par(new=TRUE)
plot(roc.lr,col="green")
par(new=TRUE)
plot(roc.nb,col="blue")

auc(roc.rf)
auc(roc.lr)
auc(roc.nb)


# Ensemble using Averaging
# Taking average of predicted probabilities
valid.df$pred_avg<-(valid.df$pred_rf.prob$X1+valid.df$pred_lr.prob$X1+valid.df$pred_nb.prob$X1)/3
#Splitting into binary classes at 0.5
valid.df$pred_class<-as.factor(ifelse(valid.df$pred_avg>0.5,"X1","X0"))
ensemble.averaging<-confusionMatrix(as.factor(valid.df$ICU_NB),valid.df$pred_class)

# Ensemble using Majority Voting
valid.df$pred_majority<-as.factor(ifelse(valid.df$pred_rf=='X1' & valid.df$pred_nb=='X1','X1',
                                         ifelse(valid.df$pred_rf=='X1' & valid.df$pred_lr=='X1','X1',
                                                ifelse(valid.df$pred_nb=='X1' & valid.df$pred_lr=='X1','X1','X0'))))
ensemble.voting<-confusionMatrix(as.factor(valid.df$ICU_NB),valid.df$pred_majority)




# Ensemble using Weighted Average
# Taking weighted average of predictions
valid.df$pred_weighted_avg<-(valid.df$pred_rf.prob$X1*0.25)+(valid.df$pred_lr.prob$X1*0.25)+(valid.df$pred_nb.prob$X1*0.5)
#Splitting into binary classes at 0.5
valid.df$pred_weighted_avg<-as.factor(ifelse(valid.df$pred_weighted_avg>0.2,'X1','X0'))
valid.df$ICU_NB_weight <- as.factor(valid.df$ICU_NB)
ensemble.weighted<-confusionMatrix(valid.df$ICU_NB_weight,valid.df$pred_weighted_avg)

con_rf<-confusionMatrix(as.factor(valid.df$ICU_NB),valid.df$pred_rf)
con_lr<-confusionMatrix(as.factor(valid.df$ICU_NB),valid.df$pred_lr)
con_nb<-confusionMatrix(as.factor(valid.df$ICU_NB),valid.df$pred_nb)

c1<-rbind("Averaging","Voting","Weighted","Random Forest", 
          "Logistic Regress", "Naive Bayes")
c2<-rbind(ensemble.averaging$overall[1],ensemble.voting$overall[1],
          ensemble.weighted$overall[1],con_rf$overall[1],
          con_lr$overall[1],con_nb$overall[1])
D1<-cbind(c1,c2)

D1