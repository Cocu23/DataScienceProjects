####################################################################
#### TPI benchmarking analysis ####
####################################################################
load("C:/Presto Workspace/R_scripts/R workspaces/new_preprocessing_v2.RData")
####################################################################
#
### CONTENT ####
#
# Create Train and Test sets and SMOTE
# Intro
# Standardization & Correlations Analysis
# Create Train and Test sets and SMOTE 
# Decision Tree
# Logistic Regression
# tbd
#
####################################################################
# Intro ####
####################################################################
library(tree)
library(caret)
library(randomForest)
library(ggplot2)
library(rpart)
library(DMwR)
library(randomForest)
library(ROCR)
library(pROC)
library(rpart)
library(RColorBrewer)
library(corrplot)
library(RODBC) 
library(sqldf)
library(data.table)
library(scales)
library(dplyr)
library(plyr)
library(zoo)
library(reshape2)
library(neuralnet)
# library(rattle)
# last package seems not to work here

#### get data 
prediction_dat<-as.data.frame(newdata)

####################################################################
# Standardization & Correlations Analysis ####
####################################################################
# format transformation ####
# change to numeric and factor only
unique(sapply(prediction_dat, class))
column<-sapply(prediction_dat, class)
z<-which(column!="numeric")
name<-colnames(prediction_dat)
z<-name[z]

# change format to numeric
for (i in 1:length(z)) {
  prediction_dat[,z[i]]<- as.numeric(prediction_dat[,z[i]])
}
# QC:
unique(sapply(prediction_dat, class))
#numeric , ok
#Checking where NAs are left
NAs_Left <- apply(prediction_dat, 2, function(x) sum(is.na(x)))
NAs_Left[NAs_Left>0] 
#none, perfecto

# check if SD alway non-zero
s=0
for (i in 1:ncol(prediction_dat)) {
  s[i]<-sd(prediction_dat[,i], na.rm = TRUE)
}
#check sd
s[s=0] 
#none!

# standardize features ####
colnames(prediction_dat)
scale_columns<-as.list(colnames(prediction_dat)[3:22])
scaled_dat<-as.data.frame(apply(prediction_dat,2, function(x) scale(x,center=TRUE, scale=TRUE)))
# reset scaled columns which schould not be standardized?
scaled_dat$Day<-prediction_dat$Day
scaled_dat$DeviceID<-prediction_dat$DeviceID
scaled_dat$newlabel<-prediction_dat$newlabel


###################################
# correlation                     #
###################################

M<-cor(prediction_dat,use="pairwise.complete.obs")

#try renaming row and column names for better display
N<-M
row.names(N)<-c(1:nrow(N))
colnames(N)<-row.names(N)
# plot it
corrplot(M, method= "circle", tl.cex=0.8) 

### find & removing highly correlated results 
correlatedCols <- findCorrelation(M, cutoff = 0.9, verbose=TRUE)
# summary of correlated pairs
# Compare row 16  and column  4 with corr  0.978 (CardReadNb / TouchNb)
# Means:  0.456 vs 0.251 so flagging column 16 

# Compare row 4  and column  6 with corr  0.972 (CardReadNb / CardWriteNb)
# Means:  0.431 vs 0.232 so flagging column 4 

# Compare row 6  and column  7 with corr  0.916 (CardWriteNb / ComFailNb)
# Means:  0.397 vs 0.215 so flagging column 6 

# Compare row 3  and column  12 with corr  0.991 (AuthentFailNb / CumulOutService)
# Means:  0.393 vs 0.194 so flagging column 3 

# Compare row 12  and column  14 with corr  0.955 (CumulOutService / CumulStandBy)
# Means:  0.346 vs 0.174 so flagging column 12 

# Compare row 20  and column  21 with corr  0.971 (transaction_sum / Heartbeat)
# Means:  0.152 vs 0.164 so flagging column 21

prediction_dat_reduced <- prediction_dat[,-c(3,4,6,16,12,21)]
scaled_dat_reduced <- scaled_dat[,-c(3,4,6,12,16,21)]

###################################
# PCA                             #
###################################
pca_data <- scaled_dat[,-which(names(scaled_dat) %in% c('DeviceID','Period','newlabel'))]
pca <- prcomp((pca_data))
summary(pca)
plot(pca)
# Explanation power is quite dispersed.
# First 5 PCs only explain 80 % of variance in data.

pca_data_reduced <- scaled_dat_reduced[,-which(names(scaled_dat_reduced) %in% c('DeviceID','Period','newlabel'))]
pca_reduced <- prcomp((pca_data_reduced))
summary(pca_reduced)
plot(pca_reduced)
# Explanation power is quite dispersed.
# First 5 PCs only explain 75 % of variance in data.
# This is in line with the reduced data space.


####################################################################
# SMOTE and Create Train and Test sets ####
####################################################################
# label has to be factor for ML-algorithms
prediction_dat$newlabel<-as.factor(prediction_dat$newlabel)
scaled_dat$newlabel<-as.factor(scaled_dat$newlabel)
prediction_dat_reduced$newlabel<-as.factor(prediction_dat_reduced$newlabel)
scaled_dat_reduced$newlabel<-as.factor(scaled_dat_reduced$newlabel)

# Set a random seed (so you will get the same results as me)
set.seed(42)

prediction_dat_smote <- prediction_dat
prediction_dat_reduced_smote <- prediction_dat_reduced
scaled_dat_smote <- scaled_dat
scaled_dat_reduced_smote <- scaled_dat_reduced

#Apply SMOTE transformation only on dynamic features, not Period of DeviceID
n <- names(prediction_dat_smote)
applySMOTEto <- as.formula(paste("newlabel ~",paste(n[!n %in% c('newlabel','DeviceID','Period')],collapse = '+')))
n <- names(prediction_dat_reduced_smote)
applySMOTEtoReduced <- as.formula(paste("newlabel ~",paste(n[!n %in% c('newlabel','DeviceID','Period')],collapse = '+')))

# Do SMOTE
prediction_dat_smote <- SMOTE(applySMOTEto, prediction_dat_smote, perc.over= 10000, perc.under = 100)
prediction_dat_reduced_smote <- SMOTE(applySMOTEtoReduced, prediction_dat_reduced_smote, perc.over= 10000, perc.under = 100)
scaled_dat_smote <- SMOTE(applySMOTEto, scaled_dat_smote, perc.over= 10000, perc.under = 100)
scaled_dat_reduced_smote <- SMOTE(applySMOTEtoReduced, scaled_dat_reduced_smote, perc.over= 10000, perc.under = 100)

# Statistics
### before SMOTE 
table(prediction_dat$newlabel)
#   0     1 
# 197447   146 
dim(prediction_dat) 
# 197593    23

#after SMOTE
table(train_data_smote$newlabel)
#   0     1 
# 11000 11110 
dim(train_data_smote)
# 22110    23

##############################################
# select dataset to use in ML algorithms     #
##############################################
mldata <- scaled_dat_reduced_smote

# create splitting index for train and test set
splitIndex<-createDataPartition(mldata$newlabel, p=.8, list=FALSE, times=1)

# sample training & test set
mltrain <- mldata[splitIndex, ]             
mltest <- mldata[-splitIndex, ]   


####################################################################
### Decision Tree ####
####################################################################
#detact neuralnet package otherwise it overrides prediction()
detach("package:neuralnet", unload=TRUE)

# create, train and test decision tree model
f <- as.formula(paste("newlabel ~",paste(n[!n %in% c('newlabel','DeviceID')],collapse = '+')))
fit_tree <- rpart(f, data= mltrain)
tree_pred <- predict(fit_tree, mltest, type="class")

# see results 
confusionMatrix(tree_pred, mltest$newlabel)
tree_pred<-as.numeric(tree_pred)
auc<- roc(mltest$newlabel, tree_pred)
print(auc)
# AUC exampleys:
# 0.881 for mldata <- prediction_dat_smote
# 0.877 for mldata <- scaled_dat_smote
# 0.876 for mldata <- scaled_dat_reduced_smote
# 0.874 for mldata <- prediction_dat_reduced_smote
# ...
# 0.534 for mldata <- scaled_dat_reduced

# ROC 
roc_pred <- prediction(tree_pred, mltest$newlabel)
par(mar = rep(2, 4))
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)

#lift curve, Sensitivity/specificity curve and precision/recall curve:
plot(performance(roc_pred, measure="lift", x.measure="rpp"), colorize=TRUE)
plot(performance(roc_pred, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred, measure="prec", x.measure="rec"), colorize=TRUE)

# Visualize tree
par(mar = rep(2, 4))
plot(fit_tree, pretty=0)
text(fit_tree, pretty=0)



####################################################################
### Random Forest ####
####################################################################
#detact neuralnet package otherwise it overrides prediction()
detach("package:neuralnet", unload=TRUE)

# create, train and test random forest model
f <- as.formula(paste("newlabel ~",paste(n[!n %in% c('newlabel','DeviceID')],collapse = '+')))

rF.trained <- randomForest(f, data=mltrain)
rF.predict <- predict(rF.trained, mltest, type="class")

# see results 
confusionMatrix(rF.predict, mltest$newlabel)
rF.predict<-as.numeric(rF.predict)
auc<- roc(mltest$newlabel, rF.predict)
print(auc)
# AUC: 0.829


# ROC 
rF.roc <- prediction(rF.predict, mltest$newlabel)
par(mar = rep(2, 4))
plot(performance(rF.roc, measure="tpr", x.measure="fpr"), colorize=TRUE)


####################################################################
### Logistic Regression ####
####################################################################
#detact neuralnet package otherwise it overrides prediction()
detach("package:neuralnet", unload=TRUE)

# create, train and test logreg model
# all features
#f <- as.formula(paste("newlabel ~",paste(n[!n %in% c('newlabel','DeviceID')],collapse = '+')))
# selected features
f <- as.formula("newlabel ~ CumulAlarm + 
                 CumulMaintenance + 
                 CumulStandBy +
                 PrintNb")

fit_logreg <- glm(f, family=binomial, data=mltrain)

# see results
summary(fit_logreg)
logreg_pred <- predict(fit_logreg, newdata=mltest, type="response")
logreg_res <- rep(0,length(logreg_pred))
logreg_res[logreg_pred>.5]<-1
#quantile(logreg_pred, c(.25, .5, .75))
confusionMatrix(logreg_res, mltest$newlabel)
pr <- prediction(logreg_res, mltest$newlabel)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
F1 <- (2 * conf$byClass[3] * conf$byClass[1]) / (conf$byClass[3] + conf$byClass[1])
auc
F1


# visualize
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)



####################################################################
# Autoencoder ####
####################################################################
require(neuralnet)

# copy data, remove period and deviceID (because categorical)
ae.tmp <- newdata
ae.tmp$Period <- NULL
ae.tmp$DeviceID <- NULL
# scale values
ae.tmp[,-which(colnames(ae.tmp) == "newlabel")]<-scale(ae.tmp[,-which(colnames(ae.tmp) == "newlabel")], center=TRUE, scale=TRUE)
ae.tmp <- as.data.frame(ae.tmp)

# use 80% of non-failures for training
ae.train <- ae.tmp[ae.tmp$newlabel == 0,]
ae.train <- ae.train[sample(nrow(ae.train)),]
ae.train.nrows <- nrow(ae.train)
ae.train <- head(ae.train,ae.train.nrows * 0.8)
# use rest of non-failures and all failures for scoring
ae.score <- tail(ae.train,ae.train.nrows * 0.2)
ae.score <- rbind(ae.score, ae.tmp[ae.tmp$newlabel != 0,])
ae.train$newlabel <- NULL

# prepare dataset for neuralnet
copy <- ae.train
colnames(copy) <- unlist(lapply(colnames(copy),function(x) paste0("out_", x)))

model <- as.formula(paste(paste(colnames(copy), collapse="+"), 
                          "~", 
                          paste(colnames(ae.train), collapse="+")))

ae.train <- cbind(ae.train, copy)

# Traing neural net with changing parameter set
trained.net <- neuralnet(model, ae.train, act.fct = "logistic", hidden = c(20), threshold = .5e+4, stepmax = 1e+3, lifesign = "full", lifesign.step = 10)


# Procesing outputs generated with sweeped parameters
net.output <- compute(trained.net, ae.score[,-which(colnames(ae.score) == "newlabel")])$net.result

scores <- rowSums((ae.score[,-which(colnames(ae.score) == "newlabel")] - net.output)^2)
ae.scored.data <- cbind(ae.score, score = scores)

ae.eval <- Reduce(rbind, lapply(unique(round(scores, 1)), function(threshold)
  {
  tp.count <- nrow(ae.scored.data[ae.scored.data$score <= threshold & ae.scored.data$newlabel == 0,])
  fp.count <- nrow(ae.scored.data[ae.scored.data$score <= threshold & ae.scored.data$newlabel != 0,])
  c(tp.count, fp.count)
}))
ae.eval <- as.data.frame(ae.eval)
colnames(ae.eval) <- c("tp.count", "fp.count")
ae.eval$tp.count <- ae.eval$tp.count / nrow(ae.scored.data[ae.scored.data$newlabel == 0,])
ae.eval$fp.count <- ae.eval$fp.count / nrow(ae.scored.data[ae.scored.data$newlabel != 0,])
plot(ae.eval$fp.count, ae.eval$tp.count, xlim=c(0,1), ylim=c(0,1))
ae.eval <- ae.eval[order(ae.eval$fp.count),]
summary(ae.eval)

auc <- (ae.eval %>% mutate(integral = lag(tp.count)*(fp.count - lag(fp.count)) + (tp.count -lag(tp.count))*(fp.count - lag(fp.count))/2) %>% tail(., nrow(ae.eval)-1)%>%colSums)[3]
auc
# current auc: 0.7570488354 (Period 30 min)
# auc: 0.6586515747 (Period 3h)

# Christian's results:
# auc1: 0.648 if hidden = c(5)
# auc2: 0.649 if hidden = c(20)
# auc3: 0.644 if hidden = c(10,5)

ae.fp = nrow(ae.scored.data[ae.scored.data$score > ae.threshold & ae.scored.data$newlabel == 0,])
ae.tp = nrow(ae.scored.data[ae.scored.data$score > ae.threshold & ae.scored.data$newlabel != 0,])


# Create new dataset from those which haven't been classified well
quantile(scores,c(.5,.75,.9))
smallscores <- head(sort(scores), 0.95*length(scores))
hist(smallscores, breaks = c(0,0.5,1,1.5,2,7))
ae.quantile = 0.9
ae.cl1.train <- ae.train[ae.]
ae.threshold <- quantile(scores, ae.quantile)
ae.threshold
ae.class1 <- ae.scored.data[ae.scored.data$score < ae.threshold,-which(names(ae.scored.data)=='score')]
ae.class2 <- ae.scored.data[ae.scored.data$score > ae.threshold,-which(names(ae.scored.data)=='score')]



# Test trained network on only the good performers
net.output <- compute(trained.net, ae.class1[,-which(colnames(ae.class1) == "newlabel")])$net.result
scores <- rowSums((ae.class1[,-which(colnames(ae.class1) == "newlabel")] - net.output)^2)
ae.class1.s <- cbind(ae.class1, score = scores)
ae.eval <- Reduce(rbind, lapply(unique(round(scores, 1)), function(threshold)
{
  tp.count <- nrow(ae.class1.s[ae.class1.s$score <= threshold & ae.class1.s$newlabel == 0,])
  fp.count <- nrow(ae.class1.s[ae.class1.s$score <= threshold & ae.class1.s$newlabel != 0,])
  c(tp.count, fp.count)
}))
ae.eval <- as.data.frame(ae.eval)
colnames(ae.eval) <- c("tp.count", "fp.count")
ae.eval$tp.count <- ae.eval$tp.count / nrow(ae.class1.s[ae.class1$newlabel == 0,])
ae.eval$fp.count <- ae.eval$fp.count / nrow(ae.class1.s[ae.class1$newlabel != 0,])
plot(ae.eval$fp.count, ae.eval$tp.count, xlim=c(0,1), ylim=c(0,1))
ae.eval <- ae.eval[order(ae.eval$fp.count),]
summary(ae.eval)

auc <- (ae.eval %>% mutate(integral = lag(tp.count)*(fp.count - lag(fp.count)) + (tp.count -lag(tp.count))*(fp.count - lag(fp.count))/2) %>% tail(., nrow(ae.eval)-1)%>%colSums)[3]
auc
# AUC: 0.546



# Test trained network on only the bad performers
net.output <- compute(trained.net, ae.class2[,-which(colnames(ae.class2) == "newlabel")])$net.result
scores <- rowSums((ae.class2[,-which(colnames(ae.class2) == "newlabel")] - net.output)^2)
ae.class2.s <- cbind(ae.class2, score = scores)
ae.eval <- Reduce(rbind, lapply(unique(round(scores, 1)), function(threshold)
{
  tp.count <- nrow(ae.class2.s[ae.class2.s$score <= threshold & ae.class2.s$newlabel == 0,])
  fp.count <- nrow(ae.class2.s[ae.class2.s$score <= threshold & ae.class2.s$newlabel != 0,])
  c(tp.count, fp.count)
}))
ae.eval <- as.data.frame(ae.eval)
colnames(ae.eval) <- c("tp.count", "fp.count")
ae.eval$tp.count <- ae.eval$tp.count / nrow(ae.class2[ae.class2$newlabel == 0,])
ae.eval$fp.count <- ae.eval$fp.count / nrow(ae.class2[ae.class2$newlabel != 0,])
plot(ae.eval$fp.count, ae.eval$tp.count, xlim=c(0,1), ylim=c(0,1))
ae.eval <- ae.eval[order(ae.eval$fp.count),]
summary(ae.eval)

auc <- (ae.eval %>% mutate(integral = lag(tp.count)*(fp.count - lag(fp.count)) + (tp.count -lag(tp.count))*(fp.count - lag(fp.count))/2) %>% tail(., nrow(ae.eval)-1)%>%colSums)[3]
auc
# AUC: 0.678


# Train new network for previously good performers
ae.class1.train <- head(ae.class1,nrow(ae.class1) * 0.8)
ae.class1.score <- tail(ae.class1,nrow(ae.class1) * 0.2)
ae.class1.train$newlabel <- NULL

copy <- ae.class1.train
colnames(copy) <- unlist(lapply(colnames(copy),function(x) paste0("out_", x)))

model <- as.formula(paste(paste(colnames(copy), collapse="+"), 
                          "~", 
                          paste(colnames(ae.class1.train), collapse="+")))

ae.class1.train <- cbind(ae.class1.train, copy)

trained.net <- neuralnet(model, ae.class1.train, act.fct = "logistic", hidden = c(20), threshold = .1e+4, stepmax = 1e+3, lifesign = "full", lifesign.step = 10)
net.output <- compute(trained.net, ae.class1[,-which(colnames(ae.class1) == "newlabel")])$net.result
scores <- rowSums((ae.class1[,-which(colnames(ae.class1) == "newlabel")] - net.output)^2)
ae.class1.s <- cbind(ae.class1, score = scores)
ae.eval <- Reduce(rbind, lapply(unique(round(scores, 1)), function(threshold)
{
  tp.count <- nrow(ae.class1.s[ae.class1.s$score <= threshold & ae.class1.s$newlabel == 0,])
  fp.count <- nrow(ae.class1.s[ae.class1.s$score <= threshold & ae.class1.s$newlabel != 0,])
  c(tp.count, fp.count)
}))
ae.eval <- as.data.frame(ae.eval)
colnames(ae.eval) <- c("tp.count", "fp.count")
ae.eval$tp.count <- ae.eval$tp.count / nrow(ae.class1.s[ae.class1$newlabel == 0,])
ae.eval$fp.count <- ae.eval$fp.count / nrow(ae.class1.s[ae.class1$newlabel != 0,])
plot(ae.eval$fp.count, ae.eval$tp.count, xlim=c(0,1), ylim=c(0,1))
ae.eval <- ae.eval[order(ae.eval$fp.count),]
summary(ae.eval)

auc <- (ae.eval %>% mutate(integral = lag(tp.count)*(fp.count - lag(fp.count)) + (tp.count -lag(tp.count))*(fp.count - lag(fp.count))/2) %>% tail(., nrow(ae.eval)-1)%>%colSums)[3]
auc
# AUC: 0.569 if hidden = c(20), threshold = .1e+4



# Train new network for previously bad performers
ae.class2.train <- head(ae.class2,nrow(ae.class2) * 0.8)
ae.class2.score <- tail(ae.class2,nrow(ae.class2) * 0.2)
ae.class2.train$newlabel <- NULL

copy <- ae.class2.train
colnames(copy) <- unlist(lapply(colnames(copy),function(x) paste0("out_", x)))

model <- as.formula(paste(paste(colnames(copy), collapse="+"), 
                          "~", 
                          paste(colnames(ae.class2.train), collapse="+")))

ae.class2.train <- cbind(ae.class2.train, copy)

trained.net <- neuralnet(model, ae.class2.train, act.fct = "logistic", hidden = c(20), threshold = .1e+4, stepmax = 1e+3, lifesign = "full", lifesign.step = 10)
net.output <- compute(trained.net, ae.class2[,-which(colnames(ae.class2) == "newlabel")])$net.result
scores <- rowSums((ae.class2[,-which(colnames(ae.class2) == "newlabel")] - net.output)^2)
ae.class2.s <- cbind(ae.class2, score = scores)
ae.eval <- Reduce(rbind, lapply(unique(round(scores, 1)), function(threshold)
{
  tp.count <- nrow(ae.class2.s[ae.class2.s$score <= threshold & ae.class2.s$newlabel == 0,])
  fp.count <- nrow(ae.class2.s[ae.class2.s$score <= threshold & ae.class2.s$newlabel != 0,])
  c(tp.count, fp.count)
}))
ae.eval <- as.data.frame(ae.eval)
colnames(ae.eval) <- c("tp.count", "fp.count")
ae.eval$tp.count <- ae.eval$tp.count / nrow(ae.class2.s[ae.class2$newlabel == 0,])
ae.eval$fp.count <- ae.eval$fp.count / nrow(ae.class2.s[ae.class2$newlabel != 0,])
plot(ae.eval$fp.count, ae.eval$tp.count, xlim=c(0,1), ylim=c(0,1))
ae.eval <- ae.eval[order(ae.eval$fp.count),]
summary(ae.eval)

auc <- (ae.eval %>% mutate(integral = lag(tp.count)*(fp.count - lag(fp.count)) + (tp.count -lag(tp.count))*(fp.count - lag(fp.count))/2) %>% tail(., nrow(ae.eval)-1)%>%colSums)[3]
auc
# AUC: 0.634 if hidden = c(5), threshold = .7e3
# AUC: 0.660 if hidden = c(20), threshold = 1e3


####################################################################

#wrap-up
####################################################################
save.image("C:/Presto Workspace/R_scripts/R workspaces/ml_anaylsis_v1.RData")
####################################################################