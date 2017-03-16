####################################################################
### Preprocessing ####
####################################################################
#
####################################################################
load("C:/Presto Workspace/R_scripts/R workspaces/kht_preprocessing_v9.RData")
# save.image("C:/Presto Workspace/R_scripts/R workspaces/kht_preprocessing_v10.RData")
####################################################################
#
############# Content ###############
#
# Intro 
# basic stats and queries
# Data format transformation 
# Feature reduction
# NA Replacement: Numeric and Categorical columns 
# Labelling 
# Investigative Data Quality
# ### Data Visualization & exploration on subsets
# Cumulative Features
# ### 1. NA Replacement
# ### 2. Monotony Check of cumulative features
#
#
#
################ not completed / next steps:
# Correlation Analysis & PCA
#
####################################################################
### Intro #### 
####################################################################

#load required libraries
library(corrplot)
library(RODBC) 
library(caret)
library(sqldf)
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(zoo)
library(reshape2)
library(tree)
library(randomForest)

# set Workspace
setwd('C://Presto Workspace/R_scripts/R workspaces')

# DB Import via ODBC:
dbhandle <- odbcDriverConnect(
  'driver={SQL Server};
  server=CALIDCSQLPWV089\\sql2016;  
  database=PRESTO_Data;
  trusted_connection=true')

# Load Data table from SQL Server
data_backup<-sqlQuery(
  dbhandle, 
  'select * from DEvice_IFS_TPI_reduced')

#create working copy
workdata<-data_backup


### Datasets
# backup: workdata_backup
# Data transformation: workdata
# feature elimination, label and Cumulatives:  reduced_data (tmp)
# elimination of NA cumuls: noncumulNA_data (tmp2)
# data with cumulative decrease analysis: data_with_drops
# final prepared dataset for decision trees: prediction_dat

# current progress (until "X")


####################################################################
### basic stats  ####
#####################################################################
# the following is not loaded into the current workspace:

# basic quality checks:
length(unique(workdata$TransactionID))-nrow(workdata)
# 0 duplicates
length(unique(workdata$DeviceID))
# 93
length(unique(workdata$DeviceLocationID))
# 46

# gather important stats in data table
column_NAs <- as.data.table(apply(workdata,2,function(x) sum(is.na(x))))
column_max <- as.data.table(apply(workdata,2,function(x) max(x, na.rm=TRUE)))
column_min <- as.data.table(apply(workdata,2,function(x) min(x, na.rm=TRUE)))
column_mean <- as.data.table(sapply(workdata,function(x) mean(x, na.rm=TRUE)))
column_sd <- as.data.table(apply(workdata,2,function(x) sd(x, na.rm=TRUE)))
# put in table
basicstats <- cbind(column_NAs,column_max,column_min,column_mean,column_sd)
colnames(basicstats) <- c('#NAs', 'Max', 'Min', 'Mean', 'Standard Deviation')
basicstats<-cbind(names(workdata),basicstats)

#clean-up
rm(column_sd, column_mean, column_min, column_max, column_NAs)
####################################################################
### Data format Transformation ####
####################################################################

#check feature data type
unique(sapply(workdata, class))
#"integer" "factor"  "numeric"

#factors had to be changed to date format
# manual transformation factor to date (function under progress)
column<-sapply(workdata, class)
x<-which(column=="factor")
name<-colnames(workdata)
y<-name[x]

# date
if(length(c<-grep("Date", y))){
  y[c]
  x[c]
}
t<-x[c]
for (i in 1:length(t)){
  workdata[,t[i]]<- as.Date(workdata[,t[i]])
}

# time
if(length(d<-grep("Time", y))){
  y[d]
  x[d]
}
tt<-x[d]
for (i in 1:length(tt)){
  workdata[,tt[i]]<- as.Date(workdata[,tt[i]])
}

# day
if(length(e<-grep("Day", y))){
  y[e]
  x[e]
}
ttt<-x[e]
for (i in 1:length(ttt)){
  workdata[,ttt[i]]<- as.Date(workdata[,ttt[i]])
}

# clean-up
rm(x,y)

####################################################################
### Feature Reduction #####
####################################################################

# still remain as factors
column<-sapply(workdata, class)
remaining<- which(column=="factor")
remaining
# FileName SPMachineID  DISVersion 

# DISVersion
# Summary: M 217 P 1619320 
workdata$DISVersion<-as.character(workdata$DISVersion)
workdata$DISVersion[workdata$DISVersion=="M"]<- 0
workdata$DISVersion[workdata$DISVersion=="P"]<- 1
workdata$DISVersion<-as.integer(workdata$DISVersion)
# due to format change replace NA with zero
workdata$DISVersion[is.na(workdata$DISVersion)]<-0
# QC:
table(workdata$DISVersion)
# works fine till here!

#create new categorical value for SPMAchineID
# revealing if ID is available or not
noID<- which(is.na(workdata$SPMachineID))
SPMID<-rep(1, nrow(workdata))
SPMID[noID]<-0
workdata$SPMachineID<-SPMID

############################################################
### Elimination of irrelevant features
############################################################

#leave feature "FileName" out for now
if ('FileName' %in% colnames(workdata)) {
  workdata$FileName<-NULL
  basicstats<-basicstats[-which(basicstats$V1=='FileName'),]
}

# still string features?
unique(sapply(workdata, class))
#"integer" "Date"   "numeric"

# create new reduced dataset with only features with less than 90% NAs of workdata
tmp<-workdata[,which(basicstats$`#NAs`<0.9*nrow(workdata))]

# general NA-Replacement function fofr IDs
makeBinary <- function(b){
  b[which(!is.na(b))] <- 1
  b[which(is.na(b))] <- 0
  return(b)
}

colnames(tmp)  
#create one binary sales features 
tmp$sales<-makeBinary(workdata$SalesType1)
table(tmp$sales)

#eliminate manually features out of interest scope
tmp$MediaSerialNumberID<-NULL
tmp$ApplicationTransactionSequenceNumber<-NULL
tmp$MsgHeaderTag<-NULL    
tmp$MsgReportDate<-NULL
tmp$MessageReceptionDate<-NULL                       
tmp$PartitionField<-NULL
tmp$TransactionReceptionBusinessDay<-NULL
tmp$AggregationDate<-NULL
tmp$OpPaymentAmount<-NULL
tmp$UnderpaymentFee<- NULL
tmp$UnderpaymentAmount<-NULL
tmp$AggregationDate<-NULL
tmp$TransactionReceptionBusinessDay<-NULL
tmp$OverdraftFee<-NULL
tmp$OverdraftValue<-NULL
tmp$EODVersionID<-NULL

# eliminate al Transaction Sequence NUmbers
TSN<-grep("TSN", names(tmp))
tmp<-tmp[,-TSN]
#eliminate revenue and sales features because sales binary feature is sufficient for now
rev<-grep("Rev", names(tmp))
colnames(tmp)[rev]  
tmp<-tmp[,-rev]
period<-grep("Period", names(tmp))
colnames(tmp)[period]
tmp<-tmp[,-period]

trx<-grep("Trx", names(tmp))
colnames(tmp)[trx]
tmp<-tmp[,-trx]

#create new binary flag variabel for FarecardID, showing if ID exists or NA
FID<- which(!(is.na(tmp$FarecardID)))
FCID<-rep(0, nrow(tmp))
FCID[FID]<-1
tmp$FarecardID<-FCID

farecard<-grep("Farecard", names(tmp))
colnames(tmp)[farecard]
#farecard ID iss the opnly one we want to keep
farecard<-farecard[-1]
tmp<-tmp[,-farecard]

#save back to new dataset
# reduced_data<-tmp
####################################################################
### labelling ####
####################################################################

#look at label feature 
nrow(tmp[which((tmp$DeviceEventCode==5 & ( tmp$DeviceMode==5))),])
# 746
unique(tmp[which(tmp$DeviceEventCode==5 ),]$DeviceMode) # 8 0 4 7 1 5
unique(tmp[!(which(tmp$DeviceEventCode==5) ),]$DeviceMode) # only NA
# Hence, DeviceMode is not NA, if DeviceEventCode equals 5 (necessary condition !)
#however, there are different event codes (then devicemode has to be NA)
unique(tmp[which(is.na(tmp$DeviceMode)==TRUE ),]$DeviceEventCode) # 6 NA  1  4  2  3

# create label (def: EventCode = 5 and DeviceMode = 5)
failure<-which((tmp$DeviceEventCode==5 & ( tmp$DeviceMode==5)))
label <- rep(0, nrow(tmp))
label[failure] <- 1

# add label to dataset
tmp$label<-label

#QC:
table(tmp$label)
#      0       1 
# 1618791     746  , ok

# test NA/non-NA cumul values to failure ratio
table(tmp[is.na(tmp$CumulAlarm),]$label) # all NAs have zero label
table(tmp[!(is.na(tmp$CumulAlarm)),]$label)
table(tmp[is.na(tmp$CumulAlarm),]$DeviceMode) #empty


###################################################################
### Investigate Data Quality Exploration ####
###################################################################

# POSTIX Time format for displaying purposes
tmp$Time <- with(tmp, as.POSIXct(paste(tmp$BusinessDay, paste(tmp$BusinessHour,tmp$BusinessQuarterHour,sep=':'))))
tmp<- tmp[,c(ncol(tmp),(1:ncol(tmp))[-ncol(tmp)])]

# check "cumulative" behaviour for different deviceIDs
# cumulative features
CumulColumns <- c('AuthentFailNb','CardReadNb',
                  'CardRWErrorNb','CardWriteNb','ComFailNb',
                  'CumulAlarm','CumulInService','CumulMaintenance',
                  'CumulOutOrder','CumulOutService','CumulStandAlone',
                  'CumulStandBy','CumulTraining', 'TouchNb', 
                  'PrintNb', 'SignOnFailNb' )
# CumulColumns<-as.list(CumulColumns)


# first example: 1190555 ####
device_1190555<-reduced_data[reduced_data$DeviceID=='1190555',]
# plotting of cumulative features
cumul_device_1190555<-device_1190555[,which(colnames(device_1190555) %in% CumulColumns)]
cumul_device_1190555$Time<-device_1190555$Time
melted_device_1190555 = melt(cumul_device_1190555, id.vars='Time')
ggplot(melted_device_1190555, aes(x=Time, y=value, colour=variable)) + geom_line() + facet_wrap(~variable, scales = "free_y")

# it seems like cumulative counts were manually resetted after maintenance
# look at specific feature in more detail
ggplot(data=cumul_device_1190555, aes(x=Time, y=CumulAlarm))+
  geom_line() +
  geom_point() 
# geom_point(aes(colour=DeviceID))  

# plotting
cumul_device_1190555<-device_1190555[,which(colnames(device_1190555) %in% CumulColumns)]
cumul_device_1190555$Time<-device_1190555$Time
melted_device_1190555 = melt(cumul_device_1190555, id.vars='Time')
ggplot(melted_device_1190555, aes(x=Time, y=value, colour=variable)) + geom_line() 

# clean-up
rm(melted_device_1190555,cumul_device_1190555, device_1190555, CumulColumns)
####################################################################
# test for second device 1190624 ####
TPI_1190624<-reduced_data[reduced_data$DeviceID=='1190624',]
cumul_TPI_1190624<-TPI_1190624[,which(colnames(TPI_1190624) %in% CumulColumns)]
cumul_TPI_1190624$Time<-TPI_1190624$Time
melted_TPI_1190624 = melt(cumul_TPI_1190624, id.vars='Time')
ggplot(melted_TPI_1190624, aes(x=Time, y=value, colour=variable)) + geom_line() + facet_wrap(~variable, scales = "free_y")
# specific view
ggplot(data=cumul_TPI_1190624, aes(x=Time, y=CumulMaintenance))+
  geom_line() +
  geom_point() 
# this device behaves like its supposed to do
rm(melted_TPI_1190624,cumul_TPI_1190624, TPI_1190624)
####################################################################

### Cumulative Features ####
####################################################################

# reset data
#reduced_data<-tmp
#tmp<-reduced_data

# general replace Categorical NA's with 0
replaceNA <- function(d){
  d[which(is.na(d))] <- -0
  return(d)
}

# Function: replace NA by last entry
replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

# columns under consideration
#  SampleColumns<- c('NewTime','Time',"BusinessDay","Day",
#                   "BusinessHour","BusinessQuarterHour",
#                   "TransactionID",'DeviceID','DeviceMode','DeviceEventCode',
#                    CumulColumns, 'label')
# tmp<-workdata[,which(colnames(workdata) %in% SampleColumns)]

#order dataset
tmp<- tmp[order(tmp$DeviceID,tmp$BusinessDay,tmp$BusinessHour, 
                tmp$BusinessQuarterHour),]

# overview:
summary(tmp[colnames(tmp) %in% CumulColumns]) # NA's   :590475 
nrow(tmp[complete.cases(tmp[,which(colnames(tmp) %in% CumulColumns)]),])
# 1029062
nrow(tmp)-1029062 # 590475, ok


####################################################################
# NA Replacement and Time Shift Cumulative Features ####
####################################################################

# create new Time (shift of day for 0-2am)
tmp <- (tmp %>% group_by(DeviceID) %>% mutate(Day = ifelse(BusinessHour <2, BusinessDay + 1, BusinessDay)))
tmp$Day<- as.Date(tmp$Day, origin="1970-01-01")
unique(tmp$Day- tmp$BusinessDay) # 0 and 1, ok
tmp$NewTime <- with(tmp, as.POSIXct(paste(tmp$Day, paste(tmp$BusinessHour,tmp$BusinessQuarterHour,sep=':'))), "%Y-%m-%d %H:%M")
#order data by new created timeline:
tmp<-tmp[order(tmp$NewTime),]

# NA replacement old####
#remove NAs in the beginning
# tmpbydevice <- tmp %>% 
#   group_by(DeviceID) %>% 
#   arrange(NewTime, TransactionID) %>% 
#   mutate_(not_all_na = paste("cummax(", paste(paste("is.na(", CumulColumns, ")", sep=""), collapse="+"), " != 16)"))
# tmp.before.nas <-filter(tmpbydevice, not_all_na != TRUE)
# tmp.after.nas <-filter(tmpbydevice, not_all_na == TRUE)
# create new working table 
#tmp2<-tmp.after.nas[order(tmp.after.nas$NewTime, tmp.after.nas$TransactionID),]
# clean up
#rm(tmp.after.nas,tmp.before.nas, tmpbydevice)

# replace NAs with last recorded non-NA value
# tmp2 <- group_by(tmp2, DeviceID)
# #tmp2 <- apply(tmp2, 2, function (x) replace_na_with_last(colnames(tmp2) %in% CumulColumns))
# tmp2 <- mutate(tmp2, CumulAlarm =replace_na_with_last(CumulAlarm))
# tmp2 <- mutate(tmp2, CumulInService = replace_na_with_last(CumulInService)) 
# tmp2 <- mutate(tmp2, CumulMaintenance = replace_na_with_last(CumulMaintenance)) 
# tmp2 <- mutate(tmp2, CumulOutOrder = replace_na_with_last(CumulOutOrder)) 
# tmp2 <- mutate(tmp2, CumulOutService = replace_na_with_last(CumulOutService)) 
# tmp2 <- mutate(tmp2, CumulStandAlone = replace_na_with_last(CumulStandAlone)) 
# tmp2 <- mutate(tmp2, CumulStandBy = replace_na_with_last(CumulStandBy)) 
# tmp2 <- mutate(tmp2, CumulTraining = replace_na_with_last(CumulTraining)) 
# tmp2 <- mutate(tmp2, AuthentFailNb = replace_na_with_last(AuthentFailNb)) 
# tmp2 <- mutate(tmp2, CardReadNb = replace_na_with_last(CardReadNb)) 
# tmp2 <- mutate(tmp2, CardRWErrorNb = replace_na_with_last(CardRWErrorNb)) 
# tmp2 <- mutate(tmp2, CardWriteNb = replace_na_with_last(CardWriteNb)) 
# tmp2 <- mutate(tmp2, ComFailNb = replace_na_with_last(ComFailNb)) 
# tmp2 <- mutate(tmp2, PrintNb = replace_na_with_last(PrintNb)) 
# tmp2 <- mutate(tmp2, SignOnFailNb = replace_na_with_last(SignOnFailNb)) 
# tmp2 <- mutate(tmp2, TouchNb = replace_na_with_last(TouchNb)) 

#Alternative:
# Interpolating does not lead to proper results due to too many NAs 
#tmp2 <- mutate(tmp2, CumulOutOrder = na.approx(CumulOutOrder))

# New NA replacement strategy #####

## remove records with NAs in cumul columns
noncumulNA_data<-tmp[complete.cases(tmp[,colnames(tmp) %in% CumulColumns]),]

#create new working table
tmp2<-noncumulNA_data
#remove old time (before shift)
tmp2$Time<-NULL
tmp2$BusinessDay<-NULL
#tmp2$BusinessHour<-NULL
#tmp2$BusinessQuarterHour<-NULL
tmp2$not_all_na<-NULL

# reorder by column index
tmp2 <- tmp2[c(41,40,2,3,1,22,4:21,23:39)]


####################################################################
# Cumulative Monotony Check####
####################################################################

#order dataset
tmp2<-tmp2[order(tmp2$NewTime, tmp2$CumulAlarm),]

# look at non-monotonically increasing cases
vars <- names(tmp2)[which(colnames(tmp2) %in% CumulColumns)]
vars <- setNames(vars, paste0(vars, "_drop"))
tmp2 <-as.data.frame(tmp2)

# flag decreases in cumulative values
detect.drops <- function(column) { 
  if(length(column) == 1) {0} else{  ifelse(column < lag(column),1,0)}
}

# create new extend table with additional features  
data_with_drops<- (tmp2 %>% group_by(DeviceID) %>% mutate_each_(funs(detect.drops), vars))

length(unique(data_with_drops$DeviceID)) #91

# get drop counts for each column of interest
colofint<-grep("_drop", colnames(data_with_drops))
summary(data_with_drops[,colofint]) 
#90 NAs each (change of DeviceID)
table(data_with_drops$CumulAlarm_drop)
# only 78 cases of decreases !

####################################################################
# Hypothesis: Stats & Visualization ####
####################################################################

# Visualization cumulative features ####
plot_data<-tmp2[,which(colnames(tmp2) %in% CumulColumns)]
plot_data$Time<-tmp2$NewTime
plot_data$DeviceID<-as.factor(tmp2$DeviceID)
#plot_data<- group_by(plot_data, DeviceID)
melted_data <- melt(plot_data, id.vars=c('Time', 'DeviceID'))
ggplot(melted_data, aes(x=Time, y=value)) + 
  geom_point(aes(colour="DeviceID"),size=0.1, alpha=0.3) +
  facet_wrap(~variable, scales = "free_y")


######## stats & feature generation ####
# additional feature generation:
my.statistics <- function(x) { tail(x, 1) }
colnames(tmp3 %>% group_by(DeviceID) %>% summarise_each_(funs(mean, sum, my.statistics), vars))

tmp2[tmp2$anomaly_CumulAlarm==1,]$Day
unique(tmp2$Day)

# stats ####
devicecount<-as.data.frame(lapply(split(tmp2, tmp2$DeviceID), function(x) nrow(x)))
devicecount<-as.data.frame(t(devicecount))
labelperdevice<- aggregate(label~DeviceID, data=tmp2, function(x) sum(x))
stats<- cbind(labelperdevice,devicecount)

colnames(stats)<-c("DeviceID", "label", "rowcount")
head(stats)

labelperday<- aggregate(label~Day, tmp2, function(x) sum(x))
length(unique(labelperday[labelperday$label>0,]$Day))
#319 out of 732 days have failure occurences




####################################################################

### Investigate Data (Time Series for labeled data) ####
####################################################################

# investigate label and devices over time
####################################################################
### get infos about Devices & labels
nrow(workdata_cumul[workdata_cumul$label==1,]) # 746
#workdata_cumul has different order than original workdata, so first find failures
failureID<-workdata_cumul[workdata_cumul$label==1,]$TransactionID
failureDevice<-workdata_cumul[workdata_cumul$label==1,]$DeviceID
failureDevice<-as.list(unique(failureDevice)) 
#88 (hence 5 devices do not have any failure)

### find devices without failures:
Devices<-as.list(unique(workdata_cumul$DeviceID))
haserror<-0
for (i in 1:length(Devices)) {
  if (Devices[i] %in% failureDevice) {
    haserror[i]<-1
  }
}

Devices[is.na(haserror)==TRUE]
#1190574, 1190588, 1190625, 1190626, 1190629
#QC:
workdata_cumul[(workdata_cumul$label==1 & workdata_cumul$DeviceID==1190574),] 
#no records, ok


# look at  failures in time series
failure_cumul<-which(workdata_cumul$label==1)
tmp_cumul<-workdata_cumul[,(1:37)]
tmp_cumul$label<-workdata_cumul$label
offset_failure<-failure_cumul-20

diff_fail<-failure_cumul-lag(failure_cumul)
failure_cumul[155:210]
min(diff_fail)
failureset<-NULL
for (i in 1:length(failure_cumul)) {
  failureset<-rbind(failureset,tmp_cumul[(offset_failure[i]:failure_cumul[i]),])
  
}
#deduplicate and print
failureset<-failureset[!duplicated(failureset),]
write.csv(failureset, "failureset.csv")
rm(failureset)
# get min and max times 
mins <-aggregate(workdata_cumul[ , "BusinessDay"]  ,list(workdata_cumul$DeviceID) , min)
maxs <-aggregate(workdata_cumul[ , "BusinessDay"]  ,list(workdata_cumul$DeviceID) , max)
colnames(mins)<- c("DeviceID","mindate")
colnames(maxs)<- c("DeviceID","maxdate")
mins$maxdate<-maxs$maxdate
rm(maxs)


####################################################################

### Visualization #########
#######################################



workdata_cumul$Month <- as.Date(cut(workdata_cumul$BusinessDay,
                                    breaks = "month"))
workdata_cumul$Week <- as.Date(cut(workdata_cumul$BusinessDay,
                                   breaks = "week"))
workdata_cumul$Day <- as.Date(cut(workdata_cumul$BusinessDay,
                                  breaks = "day"))

# distribution label per month
ggplot(data = workdata_cumul,
       aes(BusinessDay, label)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")
#limits = as.Date(c('2014-01','2016-06'))) # custom x-axis labels

# distribution devices per month
ggplot(data = workdata_cumul,
       aes(Month, DeviceID)) +
  stat_summary(fun.y = unique, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month")
#limits = as.Date(c('2014-01','2016-06'))) # custom x-axis labels

# distribution devices per week
ggplot(data = workdata_cumul,
       aes(Week, DeviceID)) +
  stat_summary(fun.y = unique, # adds up all observations for the week
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = "1 week") #

#######################################
# Device IDs ####
aggr_device<-with(workdata_cumul, tapply(DeviceID, list(BusinessDay), FUN = function(x) length(unique(x))))
deviceperday<-as.data.frame(cbind(unique(workdata_cumul$BusinessDay),aggr_device))                 
colnames(deviceperday)<- c("Day", "Distinct_Device_count")
deviceperday$Day <-as.Date(deviceperday$Day)
#plot
ggplot(deviceperday, 
       aes(Day, Distinct_Device_count)) + geom_point(stat='identity')
+scale_x_date(
  labels = date_format("%Y-%m-%d"),
  breaks = "1 month") #
#######################################
# Label ####
aggr_label<-with(workdata_cumul, tapply(label, list(BusinessDay), FUN = function (x) sum(x)))
lableperday<- as.data.frame(cbind(unique(workdata_cumul$BusinessDay), aggr_label))
colnames(lableperday)<- c("Day", "sum")
lableperday$Day <- as.Date(lableperday$Day)
#plot
ggplot(workdata_cumul, 
       aes(Day, sum)) + geom_bar(stat='identity')
+ labs(x='Day', y='sum of failures')

# find max 
max(lableperday$sum) #82
lableperday[which(lableperday$sum==82),]
#2014-11-17 16571  82

#plot
ggplot(data = workdata_cumul,
       aes(Day, label)) +
  stat_summary( fun.y= sum , # adds up all observations for the month
                geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%y-%m"),
    breaks = "month") +# custom x-axis labels
  theme(axis.text.x = element_text(size=7)) 
#######################################
# Transactions ####
aggr_trans<-with(workdata_cumul, tapply(TransactionID, list(BusinessDay), FUN = function (x) length(unique((x)))))
transperweek<- as.data.frame(cbind(unique(workdata_cumul$Day), aggr_trans))
colnames(transperweek)<- c("Day", "Transaction_Count")
transperweek$Day<-as.Date(transperweek$Day)
ggplot(data = transperweek,
       aes(Day, Transaction_Count)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "point") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") #

# find max 
max(transperdweek$Count) #26651
transperdweek[which(transperdweek$Count==max(transperdweek$Count) ),]
# Week Count
# 2016-04-25 16895 26651
busyweek<- workdata_cumul[which(workdata_cumul$Time>="2016-04-25 00:00" & workdata_cumul$Time < "2016-05-02 00:00"),]
busyspot<- busyweek[which(busyweek$DeviceEventCode==5),]
heartbeat<- workdata_cumul[which(workdata_cumul$DeviceEventCode==5),]

# plot
# Transaction count per day
ggplot(data = workdata_cumul,
       aes(Day, TransactionID)) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "point") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") #

ggplot(transperdweek, 
       aes(Week, Count)) + geom_bar(stat='identity')
+ labs(x='Day', y='sum of Transactions')


#look at label during this week
ggplot(busyweek, 
       aes(Day, label)) + geom_bar(stat='identity')
+ labs(x='Day', y='count of distinct devices')

#look at event code during this week
ggplot(data = busyweek,
       aes(Time, DeviceEventCode)) +
  stat_summary(fun.y = unique, # adds up all observations for the week
               geom = "point")  # or "line"

ggplot(busyweek, 
       aes(Time, DeviceEventCode)) + geom_bar(stat='identity')
+ labs(x='Day', y='count of distinct devices')
#######################################
# Device Event Code ####
ggplot(workdata_cumul, 
       aes(BusinessHour, DeviceEventCode)) + geom_bar(stat='identity')
+ labs(x='Day', y='count of distinct devices')
#######################################

### Correlation Analysis ####
###################################################################

# get matrix to be numeric
workdata_cor<-tmp2

unique(sapply(tmp2, class))
# "POSIXct" "POSIXt" "integer" "Date" "numeric"
# change to numeric only
column<-sapply(workdata_cor, class)
z<-which(column=="Date" | column=="integer")
name<-colnames(workdata_cor)
z<-name[z]

#integer to numeric
for (i in 1:length(z)) {
  workdata_cor[,z[i]]<- as.numeric(workdata_cor[,z[i]])
}

# remove PostIX features 
workdata_cor$Time<-NULL
workdata_cor$Week<-NULL
workdata_cor$Month<-NULL
unique(sapply(workdata_cor,class))
# numeric, excellente!


###  NA-Problem for cor ###
# since there are many NAs, we have first to deal with this
# M<-cor(workdata_cor, use="pairwise.complete.obs") 
# helps if features take different values
# however, in this case only constant value remain for multiple columns
# which lead to sd=0, hence no cor can be calculated!
################################################

#Checking where NAs are left
NAs_Left <- apply(workdata_cor, 2, function(x) sum(is.na(x)))
#get rid of all Columns with more than 1 Mio. NAs (63%)

# check if SD alway non-zero
s=0
for (i in 1:ncol(workdata_cor)) {
  s[i]<-sd(workdata_cor[,i], na.rm = TRUE)
  
}

#check sd
colnam<-names(workdata_cor)
mat<-cbind(colnam, s)
#check which sd is zero
mat[which(mat[,2]==0), ]
#[1,] "SalesTotalFee2" "0"
#[2,] "SalesType1"     "0

# remove multiple columns
#cols.dont.want <- c("SalesTotalFee2", "SalesType1") 
#workdata_cor <- workdata_cor[, ! names(workdata_cor) %in% cols.dont.want, drop = F]

# correlation
M<-cor(workdata_cor,use="pairwise.complete.obs")
N<-M
#try renaming row and column names for better display
row.names(N)<-c(1:nrow(N))
colnames(N)<-row.names(N)

corrplot(N, method= "circle") 


### removing highly correlated results 
# by Triangle Matrix
MM<-M
MM[!lower.tri(MM)] <- 0
workdata_corred<- data[,!apply(MM,2,function(x) any(x > 0.99))]

# Alternative with caret:
hc = findCorrelation(M, cutoff=0.1) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = workdata_cor[,-c(hc)]
# print (reduced_Data)

##################################################
### PCA ####
##################################################

res_workdata<-prcomp(x = workdata, scale. = TRUE)

###################################################################