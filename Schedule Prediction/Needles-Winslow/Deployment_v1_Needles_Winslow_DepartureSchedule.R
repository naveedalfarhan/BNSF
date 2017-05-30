library("sqldf")
library("lubridate")
library("dplyr")
library('stringr')
library(sqldf)
library(chron)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)
library(xts)
library(ranger)
library(caret)

setwd("H:/Schedule Prediction\Needles-Winslow/Deployment")

# Creating empty dataframe for all 3 case +1,+2,+3
Bucket=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('0-4', '4-8', '8-12', '12-16', '16-20', '20-24'))

# creating the variable for selecting the  +3/+7 Day range of the date 
day = 7

#Creating the list of all directories of Active Schedules
dirnames=c(
  'oq.tact_sch_stn.2017-03-01.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-02.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-03.02%3A00%3A03',
  'oq.tact_sch_stn.2017-03-04.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-05.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-06.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-07.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-08.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-09.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-10.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-11.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-12.01%3A00%3A01',
  'oq.tact_sch_stn.2017-03-13.02%3A00%3A02',
  'oq.tact_sch_stn.2017-03-14.02%3A00%3A04',
  'oq.tact_sch_stn.2017-03-15.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-16.02%3A00%3A02',
  'oq.tact_sch_stn.2017-03-17.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-18.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-19.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-20.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-21.02%3A00%3A04',
  'oq.tact_sch_stn.2017-03-22.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-23.02%3A00%3A04',
  'oq.tact_sch_stn.2017-03-24.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-25.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-26.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-27.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-28.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-29.02%3A00%3A00',
  'oq.tact_sch_stn.2017-03-30.02%3A00%3A01',
  'oq.tact_sch_stn.2017-03-31.02%253A00%253A00',
  'oq.tact_sch_stn.2017-04-01.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-02.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-03.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-04.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-05.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-06.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-07.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-08.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-09.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-10.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-11.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-12.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-13.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-14.02%3A00%3A02',
  'oq.tact_sch_stn.2017-04-15.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-16.02%3A00%3A01',
  'oq.tact_sch_stn.2017-04-17.02%3A00%3A00',
  'oq.tact_sch_stn.2017-04-18.02%3A00%3A05',
  'oq.tact_sch_stn.2017-04-19.00%3A00%3A02',
  'oq.tact_sch_stn.2017-04-20.00%3A00%3A01',
  'oq.tact_sch_stn.2017-04-21.00%3A00%3A00',
  'oq.tact_sch_stn.2017-04-22.00%3A00%3A00',
  'oq.tact_sch_stn.2017-04-23.00%3A00%3A00',
  'oq.tact_sch_stn.2017-04-24.00%3A00%3A02',
  'oq.tact_sch_stn.2017-04-25.00%3A00%3A01',
  'oq.tact_sch_stn.2017-04-26.00%3A00%3A02',
  'oq.tact_sch_stn.2017-04-27.00%3A00%3A03',
  'oq.tact_sch_stn.2017-04-28.00%3A00%3A02',
  'oq.tact_sch_stn.2017-04-29.00%3A00%3A01',
  'oq.tact_sch_stn.2017-04-30.00%3A00%3A03',
  'oq.tact_sch_stn.2017-05-01.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-02.00%3A00%3A00',
  'oq.tact_sch_stn.2017-05-03.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-04.00%3A00%3A02',
  'oq.tact_sch_stn.2017-05-05.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-06.00%3A00%3A00',
  'oq.tact_sch_stn.2017-05-07.00%3A00%3A00',
  'oq.tact_sch_stn.2017-05-08.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-09.00%3A00%3A02',
  'oq.tact_sch_stn.2017-05-10.00%3A00%3A00',
  'oq.tact_sch_stn.2017-05-11.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-12.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-13.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-14.00%3A00%3A03',
  'oq.tact_sch_stn.2017-05-15.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-16.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-17.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-18.00%3A00%3A01',
  'oq.tact_sch_stn.2017-05-19.00%3A00%3A01'
)

# Selecting the dirnames base on +3/+7
if (day==7)
{
  dirnames <- dirnames[50:80]
  Active_startDate = as.Date(substr(dirnames[1], 17, 26))+day
}

#Active_startDate = as.Date(substr(dirnames[1], 17, 26))+ day

#####################################################################################################################################
###############################################Filtering Out the +3/+7 Data from the raw of Active Schedule Data#####################
#####################################################################################################################################

for (f in 1:length(dirnames)){
  dir=paste0("C:/Users/ankur.sharma/Desktop/BNSF/NewData/All Active Schedules/",dirnames[f],"/oq.tact_sch_stn")
  Actv_Schd <- read.csv(dir,sep ="\t")
  
  Actv_Schd_nee_win= sqldf("Select a.*,b.* from (select * from Actv_Schd where STN_ST IN ('CA') and
                           STN_333 IN ('NEEDLES')) as a
                           INNER JOIN
                           (select * from Actv_Schd where STN_ST IN ('AZ') and
                           STN_333 IN ('WINSLOW')) as b
                           
                           ON 
                           a.TRN_TYPE=b.TRN_TYPE AND
                           a.TRN_DAY=b.TRN_DAY AND
                           a.TRN_PRTY=b.TRN_PRTY AND 
                           a.TRN_SECT =b.TRN_SECT AND
                           a.TRN_SYM=b.TRN_SYM AND
                           a.TRN_SCH_DPT_DT= b.TRN_SCH_DPT_DT
                           
                           
                           WHERE 
                           a.STN_SEQ_NBR<b.STN_SEQ_NBR
                           ")
  
  # Selecting the Required Columns
  Actv_Schd_nee_win <- Actv_Schd_nee_win[,c(1:8,12:13,15,23:25,27,28)]
  # Renaming the  columns
  colnames(Actv_Schd_nee_win) <- c("TRN_TYPE","TRN_SECT","TRN_SYM", "TRN_DAY", "TRN_SCH_DPT_DT","NEE_STN_SEQ_NBR",  
                                   "NEE_STN_333", "NEE_STN_ST",   "NEE_EST_DPT_DT","NEE_EST_DPT_TM","TRN_PRTY",  
                                   "WIN_STN_SEQ_NBR",  
                                   "WIN_STN_333", "WIN_STN_ST","WIN_EST_ARR_DT", "WIN_EST_ARR_TM")
  
  
  # Changing the date and time format
  Actv_Schd_nee_win$NEE_EST_DPT_DT = as.Date(Actv_Schd_nee_win$NEE_EST_DPT_DT, format = '%d%b%Y')
  Actv_Schd_nee_win$NEE_EST_DPT_TM <- times(Actv_Schd_nee_win$NEE_EST_DPT_TM)
  Actv_Schd_nee_win$WIN_EST_ARR_DT = as.Date(Actv_Schd_nee_win$WIN_EST_ARR_DT, format = '%d%b%Y')
  Actv_Schd_nee_win$WIN_EST_ARR_TM <- times(Actv_Schd_nee_win$WIN_EST_ARR_TM)
  
  
  #Selecting the Active Schedule data based on day(+3/+7)
  temp = as.Date(substr('oq.tact_sch_stn.2017-05-19.00%3A00%3A01', 17, 26)) + day
  Actv_Schd_nee_win<-Actv_Schd_nee_win %>% filter(NEE_EST_DPT_DT==(temp))
  
  
  # Taking Departure hours from the Departure time
  Actv_Schd_nee_win$NEE_Departure_hour <- as.character(Actv_Schd_nee_win$NEE_EST_DPT_TM)
  Actv_Schd_nee_win$NEE_Departure_hour <- as.numeric(substring(Actv_Schd_nee_win$NEE_Departure_hour, 1,2))
  
  
  # Bucketing the Departure hours
  Actv_Schd_nee_win$Departure_Time_Bucket <- cut(Actv_Schd_nee_win$NEE_Departure_hour, breaks=c(-1,4,8,12,16,20,25), labels=c("0-4","4-8","8-12", "12-16", "16-20", "20-24"))
  Result <- as.data.frame.matrix(table(Actv_Schd_nee_win$NEE_EST_DPT_DT,Actv_Schd_nee_win$Departure_Time_Bucket))
  Bucket=rbind(Bucket,Result)
  
  
  # Calculating the Transit Time
  Actv_Schd_nee_win$DPT_TIME_STAMP = as.POSIXct(paste(Actv_Schd_nee_win$NEE_EST_DPT_DT, Actv_Schd_nee_win$NEE_EST_DPT_TM),format="%Y-%m-%d %H:%M:%S")
  Actv_Schd_nee_win$ARR_TIME_STAMP = as.POSIXct(paste(Actv_Schd_nee_win$WIN_EST_ARR_DT, Actv_Schd_nee_win$WIN_EST_ARR_TM),format="%Y-%m-%d %H:%M:%S")
  
  Actv_Schd_nee_win$Transit_Time <- difftime(Actv_Schd_nee_win$ARR_TIME_STAMP, Actv_Schd_nee_win$DPT_TIME_STAMP, units = "hours")
  Actv_Schd_nee_win$Transit_Time_Bucket <- ceiling(Actv_Schd_nee_win$Transit_Time/4)
  
  #Appending the all files into 1
  if (f==1){
    Addcolnames=TRUE
    appendfile=FALSE
  }else{
    Addcolnames=FALSE
    appendfile=TRUE
  }
  
  filedate = substr(dirnames[f], 17, 26)
  Actv_Schd_nee_win$FileDate=as.Date(filedate)
  
  print (paste("Filedate:",filedate))
  print (paste("+",day, "date:",temp))
  print (paste("dim of DAta:",dim(Actv_Schd_nee_win)))
  print("----------------------------------------------------------")
  write.table(x=Actv_Schd_nee_win,paste0("Day+",day,"_ActiveData_Needles_Winslow_v4.csv"),append=appendfile,col.names =Addcolnames,sep=",", row.names = FALSE )
  
}

write.csv(Bucket, paste0("Day+",day,"_bucketWiseSplit_v4.csv"))
TDPS4=  c("TRN_TYPE","TRN_SYM", "TRN_DAY", "TRN_PRTY", "TRN_SECT","TRN_SCH_DPT_DT","NEE_STN_SEQ_NBR","WIN_STN_SEQ_NBR") 

# Reading the Active Final Prepare Date 
ActiveSchedule=read.csv(paste0("Day+",day,"_ActiveData_Needles_Winslow_v4.csv"))

# Changing the Date format of TRN_SCH_DPT_DT
ActiveSchedule$TRN_SCH_DPT_DT=as.Date(ActiveSchedule$TRN_SCH_DPT_DT, format = '%d%b%Y')
ActiveSchedule$TRN_SCH_DPT_DT=as.Date(ActiveSchedule$TRN_SCH_DPT_DT, format = "%m/%d/%Y")
ActiveSchedule$Departure_Time_Bucket=paste0("'",ActiveSchedule$Departure_Time_Bucket)

dim(ActiveSchedule)
dim(unique(ActiveSchedule[,TDPS4]))

#############  Removing the Duplicates from the Prepare data of Active Schedule    #############

# Grouping the data based on TDPS4
query="select TRN_TYPE,TRN_SYM, TRN_DAY, TRN_PRTY, TRN_SECT,TRN_SCH_DPT_DT,NEE_STN_SEQ_NBR,WIN_STN_SEQ_NBR,
count(*)as freq from ActiveSchedule
group by TRN_TYPE,TRN_SYM, TRN_DAY, TRN_PRTY,TRN_SECT,TRN_SCH_DPT_DT,NEE_STN_SEQ_NBR,WIN_STN_SEQ_NBR"
RowCounts=sqldf(query)


#Ordering the data based on frequency
ss=merge(ActiveSchedule,RowCounts)
ss=ss[with( ss, order(-freq, TRN_TYPE,TRN_SYM, TRN_DAY, TRN_PRTY, TRN_SECT,TRN_SCH_DPT_DT)),]

#Removing the duplicates based on TDPS4
ss=ss[with( ss, order(TRN_TYPE,TRN_SYM, TRN_DAY, TRN_PRTY, TRN_SECT,TRN_SCH_DPT_DT)),]
ActiveSchedule_dedup=ss[!duplicated(ss[c("TRN_TYPE","TRN_SYM", "TRN_DAY", "TRN_PRTY", "TRN_SECT","TRN_SCH_DPT_DT")]),]
dim(ActiveSchedule_dedup)
#Saving the Active Scedule Data
write.csv(ActiveSchedule_dedup, paste0("ActiveSchedule_Day+",day,"_dedup_Needles_Winslow_v4.csv"))


#####################################################################################################################################
########################################## Actual Schhedule Data Cleaning and Filtering #############################################
#####################################################################################################################################

ActualSchedule=read.csv('Needles_Winslow_Actual_30May.csv')

#Taking only the required columns which are present in Active
ActualSchedule<- ActualSchedule[,c(1:5,8,19:20,23,24,27,32,135,146:147,151)]
# [1] "TRN_TYPE"       "TRN_SECT"      
# [3] "TRN_SYM"        "TRN_DAY"       
# [5] "TRN_PRTY"       "LST_RPTG_333"  
# [7] "EVT_DT"         "EVT_TM"        
# [9] "TRN_SCH_DPT_DT" "STN_SEQ_NBR"   
# [11] "TRN_SVC_TYP"    "TRN_RPTG_GRP"  
# [13] "LST_RPTG_333.1" "EVT_DT.1"      
# [15] "EVT_TM.1"       "STN_SEQ_NBR.1"

#Renaming the columns
colnames(ActualSchedule) <- c("TRN_TYPE","TRN_SECT","TRN_SYM", "TRN_DAY", "TRN_PRTY","NEE_LST_RPTG_333","NEE_EVT_DT","NEE_EVT_TM", 
                              "TRN_SCH_DPT_DT","NEE_STN_SEQ_NBR","TRN_SVC_TYP","TRN_RPTG_GRP","WIN_LST_RPTG_333","WIN_EVT_DT","WIN_EVT_TM","WIN_STN_SEQ_NBR")

# Changing into Date Format
ActualSchedule$TRN_SCH_DPT_DT=as.Date(ActualSchedule$TRN_SCH_DPT_DT,format = "%m/%d/%Y")

#calculating the Departure bucket
ActualSchedule$Departure_Date = as.Date(ActualSchedule$NEE_EVT_DT, format = '%m/%d/%Y')
ActualSchedule$Departure_Time <- times(gsub("\\.", ":", ActualSchedule$NEE_EVT_TM))
ActualSchedule$Departure_hour <- as.character(ActualSchedule$NEE_EVT_TM)
ActualSchedule$Departure_hour <- as.numeric(substring(ActualSchedule$Departure_hour, 1,2))
ActualSchedule$Departure_Time_Bucket <- cut(ActualSchedule$Departure_hour, breaks=c(-1,4,8,12,16,20,25), labels=c("'0-4","'4-8","'8-12", "'12-16", "'16-20", "'20-24"))

# Cleaning the Station Sequence Number
ActualSchedule$Departure_Date=as.Date(ActualSchedule$Departure_Date,format = "%m/%d/%Y")
ActualSchedule$NEE_STN_SEQ_NBR=as.numeric(gsub(",", "", ActualSchedule$NEE_STN_SEQ_NBR))
ActualSchedule$WIN_STN_SEQ_NBR=as.numeric(gsub(",", "", ActualSchedule$WIN_STN_SEQ_NBR))

#Calculating Transit time
ActualSchedule$DPT_TIME_STAMP = as.POSIXct(paste(ActualSchedule$NEE_EVT_DT, ActualSchedule$NEE_EVT_TM),format="%m/%d/%Y %H.%M.%S")
ActualSchedule$ARR_TIME_STAMP = as.POSIXct(paste(ActualSchedule$WIN_EVT_DT, ActualSchedule$WIN_EVT_TM),format="%m/%d/%Y %H.%M.%S")
ActualSchedule$Transit_Time <- difftime(ActualSchedule$ARR_TIME_STAMP, ActualSchedule$DPT_TIME_STAMP, units = "hours")
ActualSchedule$Transit_Time_Bucket <- ceiling(ActualSchedule$Transit_Time/4) ## each bucket is of  4 hours

#Hardcoded Variables - Uncomment when running from beginning -Important
temp = as.Date("2017-05-26") ## change name to something more descriptive
Active_startDate = as.Date("2017-04-26")
#######################################################################

#Saving the Actual Schedule Data
ActualSchedule=  ActualSchedule[ActualSchedule$Departure_Date<=as.Date(temp-7,format = "%Y-%m-%d"),] ## 7=?
ActualSchedule=  ActualSchedule[ActualSchedule$Departure_Date>=as.Date(Active_startDate,format = "%Y-%m-%d"),]
write.csv(ActualSchedule, paste0("ActualSchedule_Needles_Winslow_v4.csv"))


#####################################################################################################################################
######################################  Merge Active Schedule and Actual Schedule and Adding more fields ############################
#####################################################################################################################################
AllData=merge(ActiveSchedule_dedup, ActualSchedule, all=TRUE, suffix=c("._actv","._act"),by=TDPS4)
# c('TRN_TYPE','TRN_PRTY','TRN_SECT', 'TRN_SYM','TRN_SCH_DPT_DT','STN_SEQ_NBR')
# NOte 'STN_SEQ_NBR' is also used here

# Check how many are common to both files. 
dim(AllData)
dim(ActualSchedule)
dim(ActiveSchedule_dedup)
# This nnumber should be < min(dim of actual and active)
dim(AllData[(!is.na(AllData$Departure_Time_Bucket._act))&(!is.na(AllData$Departure_Time_Bucket._actv)),])

# Adding extra metrics to the data
bucket_numeric=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c('Departure_bucket', 'Number'))
Departure_bucket = c("'0-4","'4-8","'8-12", "'12-16", "'16-20", "'20-24")
for(i in 1:6) {
  bucket_numeric[i,1] = Departure_bucket[i]
  bucket_numeric[i,2] = i
}

# Calculating the new matrix (INBoth,inActualNotActive, inActiveNotActuals etc.)
AllData$Departure_Time_Bucket._actv_numeric <- bucket_numeric$Number[match(AllData$Departure_Time_Bucket._actv, bucket_numeric$Departure_bucket)]
AllData$Departure_Time_Bucket._act_numeric <- bucket_numeric$Number[match(AllData$Departure_Time_Bucket._act, bucket_numeric$Departure_bucket)]
AllData$NEE_EVT_TM <- gsub("\\.", ":", AllData$NEE_EVT_TM)
AllData$NEE_EVT_DT = as.Date(AllData$NEE_EVT_DT, format = '%m/%d/%Y')
AllData$ActDepTime = as.POSIXct(paste(AllData$NEE_EVT_DT, AllData$NEE_EVT_TM),format="%Y-%m-%d%H:%M:%S")
AllData$ActiveDepTime = as.POSIXct(paste(AllData$NEE_EST_DPT_DT, AllData$NEE_EST_DPT_TM),format="%Y-%m-%d%H:%M:%S")
AllData$DevianceInCalDates  = as.Date(as.character(AllData$NEE_EVT_DT), format="%Y-%m-%d")-as.Date(as.character(AllData$NEE_EST_DPT_DT), format="%Y-%m-%d")
AllData$DevianceInBuckets = (AllData$Departure_Time_Bucket._act_numeric- AllData$Departure_Time_Bucket._actv_numeric) + 6 *  AllData$DevianceInCalDates
AllData$Actual=ifelse(is.na(AllData$Departure_Time_Bucket._act),0,1)
AllData$Active=ifelse(is.na(AllData$Departure_Time_Bucket._actv),0,1)
AllData$InBoth = if_else((AllData$Actual==1 & AllData$Active==1),1,0)
AllData$inActualNotActive = if_else((AllData$Actual==1 & AllData$Active==0),1,0)
AllData$inActiveNotActuals = if_else((AllData$Active==1 & AllData$Actual==0),1,0)

# Saving the  Merge data of Active and Actual Schedule 
write.csv(AllData,paste0("Active_Actual_Needles_Winslow_Day+",day,"_v4.csv"),row.names = FALSE)
saveRDS(AllData , paste0("RDS1Active_Actual_Needles_Winslow_Day+",day,"_v4.RDS"))

#Removing all The variables
rm(list= ls()[!(ls() %in% c('AllData','day','temp'))])

#####################################################################################################################################
######################################  Creating the Merge data on Date and Bucket level ###########################################
#####################################################################################################################################
setwd("H:/Schedule Prediction/Needles-Winslow/Deployment")
day =7
# Reading the Merge RDS file 
AllData=readRDS(paste0("RDS1Active_Actual_Needles_Winslow_Day+",day,"_v4.RDS"))

# Agrregating the data on date and Bucket level
actuals=AllData %>% filter(Actual ==1) %>%group_by(NEE_EVT_DT,Departure_Time_Bucket._act) %>% summarise(actuals=n())
colnames(actuals)<- c("Date","Bucket","actuals")
active=AllData %>% filter(Active ==1) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(active=n())
colnames(active)<- c("Date","Bucket","active")
active$Bucket=gsub("\'\'","\'",active$Bucket)
ScheduledbutNeverCame=AllData %>% filter(inActiveNotActuals ==1) %>% group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(ScheduledbutNeverCame=n())
colnames(ScheduledbutNeverCame)<- c("Date","Bucket","ScheduledbutNeverCame")
Camebutneverscheduled=AllData %>% filter(inActualNotActive ==1) %>%group_by(NEE_EVT_DT,Departure_Time_Bucket._act) %>% summarise(Camebutneverscheduled=n())
colnames(Camebutneverscheduled)<- c("Date","Bucket","Camebutneverscheduled")
ScheduledTodayCameToday=AllData %>% filter(InBoth ==1 & DevianceInCalDates==0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(ScheduledTodayCameToday=n())
colnames(ScheduledTodayCameToday)<- c("Date","Bucket","ScheduledTodayCameToday")
sameBucket=AllData %>% filter(InBoth ==1 & DevianceInCalDates==0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(DevianceInBuckets = sum(DevianceInBuckets==0))
colnames(sameBucket)<- c("Date","Bucket","sameBucket")
previousBucket=AllData %>% filter(InBoth ==1 & DevianceInCalDates==0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(DevianceInBuckets = sum(DevianceInBuckets>0))
colnames(previousBucket)<- c("Date","Bucket","previousBucket")
laterBucket=AllData %>% filter(InBoth ==1 & DevianceInCalDates==0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(DevianceInBuckets = sum(DevianceInBuckets<0))
colnames(laterBucket)<- c("Date","Bucket","laterBucket")
ScheduledTodaycamelater=AllData %>% filter(InBoth ==1 & DevianceInCalDates>0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(ScheduledTodaycamelater=n())
colnames(ScheduledTodaycamelater)<- c("Date","Bucket","ScheduledTodaycamelater")
ScheduledTodayCameYesterday=AllData %>% filter(InBoth ==1 & DevianceInCalDates<0) %>%group_by(NEE_EST_DPT_DT,Departure_Time_Bucket._actv) %>% summarise(ScheduledTodayCameYesterday=n())
colnames(ScheduledTodayCameYesterday)<- c("Date","Bucket","ScheduledTodayCameYesterday")
CameTodayScheduledEarlier=AllData %>% filter(InBoth ==1 & DevianceInBuckets>0) %>%group_by(NEE_EVT_DT,Departure_Time_Bucket._act) %>% summarise(CameTodayScheduledEarlier=n())
colnames(CameTodayScheduledEarlier)<- c("Date","Bucket","CameTodayScheduledEarlier")
CameTodayScheduledLater=AllData %>% filter(InBoth ==1 & DevianceInBuckets<0) %>%group_by(NEE_EVT_DT,Departure_Time_Bucket._act) %>% summarise(CameTodayScheduledLater=n())
colnames(CameTodayScheduledLater)<- c("Date","Bucket","CameTodayScheduledLater")

# creating the base of Date and Bucket to get all thee aggregating data into one shape
baseDT <- sort(na.exclude(unique(AllData$NEE_EVT_DT)))
baseBkt <- na.exclude(unique(AllData$Departure_Time_Bucket._act))
base <- as.data.frame(expand.grid(baseDT, baseBkt))
colnames(base)<- c("Date","Bucket")

# Merging with the base
AddedRemovedTrains = merge(base, actuals, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, active, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, ScheduledbutNeverCame, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, Camebutneverscheduled, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, ScheduledTodayCameToday, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, sameBucket, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, previousBucket, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, laterBucket, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, ScheduledTodaycamelater, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, ScheduledTodayCameYesterday, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, CameTodayScheduledEarlier, all.x = T)
AddedRemovedTrains = merge(AddedRemovedTrains, CameTodayScheduledLater, all.x = T)

# forecast Variables
active_7pred <-active
active_7pred$Date=as.Date(active_7pred$Date,format = "%Y-%m-%d")
active_7pred  <-  active_7pred[active_7pred$Date>as.Date(temp-7,format = "%Y-%m-%d"),]
# creating the base of Date and Bucket to get all thee aggregating data into one shape
baseDT <- sort(na.exclude(unique(active_7pred$Date)))
baseBkt <- na.exclude(unique(active_7pred$Bucket))
base <- as.data.frame(expand.grid(baseDT, baseBkt))
colnames(base)<- c("Date","Bucket")
AddedRemovedTrains_7Pred = merge(base, active_7pred, all.x = T)

#Early-Delayed Train Variables
Delayed_Trains <- AllData %>% filter(InBoth ==1)
hist(as.numeric(Delayed_Trains$DevianceInBuckets))
summary(as.factor(Delayed_Trains$DevianceInBuckets))
Delayed_Trains$BKT_DEV_BKT <- cut(as.numeric(Delayed_Trains$DevianceInBuckets), breaks=c(-100,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,100), labels=c("<=-2","-1","0","1","2","3","4",">=5"))
table(Delayed_Trains$BKT_DEV_BKT)
Delayed_Trains <- Delayed_Trains[,c("NEE_EST_DPT_DT", "Departure_Time_Bucket._actv", "BKT_DEV_BKT")]
Delayed_Trains <- Delayed_Trains %>% group_by(NEE_EST_DPT_DT, Departure_Time_Bucket._actv, BKT_DEV_BKT) %>% summarise(count= n())
Delayed_Trains<- spread(Delayed_Trains, BKT_DEV_BKT, count)
Delayed_Trains[is.na(Delayed_Trains)] <- 0
colnames(Delayed_Trains) <- c("Date","Bucket", "LTE_minus2_Early", "minus1_Early", "On_Time",
                              "Plus1_Delay", "Plus2_Delay", "Plus3_Delay",
                              "Plus4_Delay", "GTE_Plus5_Delay" )

#Merging Delayed Train Variables
AddedRemovedTrains = merge(AddedRemovedTrains, Delayed_Trains, all.x = T)

#PRE-POST Train Variables
pre_post_Trains <- AllData %>% filter(InBoth ==1)
hist(as.numeric(pre_post_Trains$DevianceInBuckets))
summary(as.factor(pre_post_Trains$DevianceInBuckets))
pre_post_Trains$BKT_DEV_BKT <- cut(as.numeric(pre_post_Trains$DevianceInBuckets), breaks=c(-100,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,100), labels=c("LTE_minus2_Pre", "Pre_minus1_Pre", "On_Time","Plus1_Post", "Plus2_Post", "Plus3_Post","Plus4_Post", "GTE_Plus5_Post"))
table(pre_post_Trains$BKT_DEV_BKT)
pre_post_Trains <- pre_post_Trains[,c("NEE_EVT_DT", "Departure_Time_Bucket._act", "BKT_DEV_BKT")]
pre_post_Trains <- pre_post_Trains %>% group_by(NEE_EVT_DT, Departure_Time_Bucket._act, BKT_DEV_BKT) %>% summarise(count= n())
pre_post_Trains<- spread(pre_post_Trains, BKT_DEV_BKT, count)
pre_post_Trains[is.na(pre_post_Trains)] <- 0
pre_post_Trains <- pre_post_Trains %>% select(-c(On_Time))
colnames(pre_post_Trains)[1:2] <- c("Date", "Bucket")

#merging pre-post Train Variables
AddedRemovedTrains = merge(AddedRemovedTrains, pre_post_Trains, all.x = T)

###########################################################################################################################
########################################  Adding PREDICTORS  ##############################################################
############################################################################################################################
Predictor_Variables <- AllData %>% filter(Active ==1)

#Predictor - TYPE
table(Predictor_Variables$TRN_TYPE)
Pred_Type <- Predictor_Variables[,c("NEE_EST_DPT_DT", "Departure_Time_Bucket._actv", "TRN_TYPE")]
Pred_Type <- Pred_Type %>% group_by(NEE_EST_DPT_DT, Departure_Time_Bucket._actv, TRN_TYPE) %>% summarise(count= n())
Pred_Type<- spread(Pred_Type, TRN_TYPE, count)
Pred_Type[is.na(Pred_Type)] <- 0
colnames(Pred_Type) <- paste0("TYPE_",colnames(Pred_Type))
colnames(Pred_Type)[1] <- "Date"
colnames(Pred_Type)[2] <- "Bucket"
#Merging Type predictor variables
AddedRemovedTrains = merge(AddedRemovedTrains, Pred_Type, all.x = T)
AddedRemovedTrains_7Pred = merge(AddedRemovedTrains_7Pred, Pred_Type, all.x = T)

#Predictor - SYM
table(Predictor_Variables$TRN_SYM)
Pred_SYM <- Predictor_Variables[,c("NEE_EST_DPT_DT", "Departure_Time_Bucket._actv", "TRN_SYM")]
Pred_SYM <- Pred_SYM %>% group_by(NEE_EST_DPT_DT, Departure_Time_Bucket._actv, TRN_SYM) %>% summarise(count= n())
Pred_SYM<- spread(Pred_SYM, TRN_SYM, count)
Pred_SYM[is.na(Pred_SYM)] <- 0
colnames(Pred_SYM) <- paste0("SYM_",colnames(Pred_SYM))
colnames(Pred_SYM)[1] <- "Date"
colnames(Pred_SYM)[2] <- "Bucket"
#Merging SYM predictor variables
AddedRemovedTrains = merge(AddedRemovedTrains, Pred_SYM, all.x = T)
AddedRemovedTrains_7Pred = merge(AddedRemovedTrains_7Pred, Pred_SYM, all.x = T)

#Predictor - PRIORITY
table(Predictor_Variables$TRN_PRTY)
Pred_PRTY <- Predictor_Variables[,c("NEE_EST_DPT_DT", "Departure_Time_Bucket._actv", "TRN_PRTY")]
Pred_PRTY <- Pred_PRTY %>% group_by(NEE_EST_DPT_DT, Departure_Time_Bucket._actv, TRN_PRTY) %>% summarise(count= n())
Pred_PRTY<- spread(Pred_PRTY, TRN_PRTY, count)
Pred_PRTY[is.na(Pred_PRTY)] <- 0
colnames(Pred_PRTY) <- paste0("PRTY_",colnames(Pred_PRTY))
colnames(Pred_PRTY)[1] <- "Date"
colnames(Pred_PRTY)[2] <- "Bucket"
#Merging SYM predictor variables
AddedRemovedTrains = merge(AddedRemovedTrains, Pred_PRTY, all.x = T)
AddedRemovedTrains_7Pred = merge(AddedRemovedTrains_7Pred, Pred_PRTY, all.x = T)

#Predictor - SECTION
table(Predictor_Variables$TRN_SECT)
Pred_SECT <- Predictor_Variables[,c("NEE_EST_DPT_DT", "Departure_Time_Bucket._actv", "TRN_SECT")]
Pred_SECT <- Pred_SECT %>% group_by(NEE_EST_DPT_DT, Departure_Time_Bucket._actv, TRN_SECT) %>% summarise(count= n())
Pred_SECT<- spread(Pred_SECT, TRN_SECT, count)
Pred_SECT[is.na(Pred_SECT)] <- 0
colnames(Pred_SECT) <- paste0("SECT_",colnames(Pred_SECT))
colnames(Pred_SECT)[1] <- "Date"
colnames(Pred_SECT)[2] <- "Bucket"
#Merging SYM predictor variables
AddedRemovedTrains = merge(AddedRemovedTrains, Pred_SECT, all.x = T)
AddedRemovedTrains_7Pred = merge(AddedRemovedTrains_7Pred, Pred_SECT, all.x = T)




####################################################################################################################
##########################################Adding transit buckets variables ########################################
####################################################################################################################

Transit = AllData %>% filter(Actual == 1) %>%group_by(NEE_EVT_DT,Departure_Time_Bucket._act) %>% 
  summarise(Transit_Bucket_2B=sum(Transit_Time_Bucket._act ==2),
            Transit_Bucket_3B=sum(Transit_Time_Bucket._act ==3),
            Transit_Bucket_4B=sum(Transit_Time_Bucket._act ==4),
            Transit_Bucket_5B=sum(Transit_Time_Bucket._act ==5),
            Transit_Bucket_6B=sum(Transit_Time_Bucket._act ==6),
            Transit_Bucket_GT7B=sum(Transit_Time_Bucket._act >6))
colnames(Transit)[1:2]<- c("Date","Bucket")
AddedRemovedTrains = merge(AddedRemovedTrains, Transit, all.x = T)

#Data Formating
# Adding timeseries variable, ordering and replacing NAs by 0
a <- data.frame("Bucket"=unique(AddedRemovedTrains$Bucket), "hr"= c(4,8,12,16,20,24))
AddedRemovedTrains <- merge(AddedRemovedTrains,a)
AddedRemovedTrains$TS_Bucket = as.POSIXct(paste(AddedRemovedTrains$Date, AddedRemovedTrains$hr),format="%Y-%m-%d%H")
AddedRemovedTrains[is.na(AddedRemovedTrains)] <- 0
AddedRemovedTrains$weekday = weekdays(AddedRemovedTrains$Date)
AddedRemovedTrains$Bucket  <- as.character(gsub("'","BUCKET_",AddedRemovedTrains$Bucket))
AddedRemovedTrains$Bucket  <- as.character(gsub("-","_",AddedRemovedTrains$Bucket))
AddedRemovedTrains$Bucket  <- as.character(gsub("0_4","00_04",AddedRemovedTrains$Bucket))
AddedRemovedTrains$Bucket  <- as.character(gsub("4_8","04_08",AddedRemovedTrains$Bucket))
AddedRemovedTrains$Bucket  <- as.character(gsub("8_12","08_12",AddedRemovedTrains$Bucket))

AddedRemovedTrains_7Pred$weekday = weekdays(AddedRemovedTrains_7Pred$Date)
AddedRemovedTrains_7Pred$Bucket  <- as.character(gsub("'","BUCKET_",AddedRemovedTrains_7Pred$Bucket))
AddedRemovedTrains_7Pred$Bucket  <- as.character(gsub("-","_",AddedRemovedTrains_7Pred$Bucket))
AddedRemovedTrains_7Pred$Bucket  <- as.character(gsub("0_4","00_04",AddedRemovedTrains_7Pred$Bucket))
AddedRemovedTrains_7Pred$Bucket  <- as.character(gsub("4_8","04_08",AddedRemovedTrains_7Pred$Bucket))
AddedRemovedTrains_7Pred$Bucket  <- as.character(gsub("8_12","08_12",AddedRemovedTrains_7Pred$Bucket))

# Saving the Result
AddedRemovedTrains <- AddedRemovedTrains[,c((ncol(AddedRemovedTrains)-1),2,1,ncol(AddedRemovedTrains),3:(ncol(AddedRemovedTrains)-3))]
write.csv(AddedRemovedTrains,paste0("AddedRemovedTrains_Day+",day,"_v4.csv"))
saveRDS(AddedRemovedTrains, paste0("RDS2_Aggregated_Needles_Winslow_Day+",day,"_v4.RDS"))

#Removing all The variables
rm(list= ls()[!(ls() %in% c('AddedRemovedTrains','day','AddedRemovedTrains_7Pred','temp'))])


###########################################################################################################################
########################################  Prediction  ####################################################################
############################################################################################################################

#Reading the Aggregateed Needles Winslow
train <-readRDS(paste0("RDS2_Aggregated_Needles_Winslow_Day+",day,"_v4.RDS"))
pred_variables <- AddedRemovedTrains_7Pred
pred_variables[is.na(pred_variables)] <- 0

train=train[with(train, order(Date,Bucket)), ]
train_charact <- colnames(train)[32:(ncol(train)-6)]
train_charact <- c("Bucket","weekday",train_charact)



#############Bucket mean for Deleted and Added Trains################################

train_bucket_means <- train %>% group_by(Bucket) %>% summarise(Pred_Deleted = mean(ScheduledbutNeverCame), Pred_Added = mean(Camebutneverscheduled))
pred_variables <- merge(pred_variables, train_bucket_means)
pred_variables<- pred_variables[with(pred_variables, order(Date,Bucket)), ]

##############Linear Regression for ON-SCHEDULE Trains#########################################
fit <- lm(sameBucket ~ active * Bucket + weekday , data = train)
fcast <- forecast(fit, newdata = pred_variables)
pred_variables$Pred_S0D0SB <- fcast$mean


########################################################################################################
######################Predict Delayed Variables#########################################################
########################################################################################################

models <- c("LTE_minus2_Early", "minus1_Early", "On_Time","Plus1_Delay","Plus2_Delay", "Plus3_Delay",               
            "Plus4_Delay","GTE_Plus5_Delay")
train$Early <- train$LTE_minus2_Early + train$minus1_Early
train$Delay <- (train$Plus1_Delay + train$Plus2_Delay + train$Plus3_Delay +
                  train$Plus4_Delay + train$GTE_Plus5_Delay)
models <- c(models,"Early", "Delay")

#RMSE = c()
for (i in models){
  print(i)
  train_dynamic <- train[,c(i, train_charact)]
  fit <- train(
    as.formula(paste(i,"~ ."))
    ,data = train_dynamic
    ,method = "ranger"
    ,trControl = trainControl(method="cv", number = 5, allowParallel = TRUE, verbose = TRUE)
    #,tuneGrid = expand.grid(mtry = 4)
    ,importance = 'impurity'
    ,min.node.size = 5
  )
  #accuracy
  fcast = predict(fit, pred_variables)
  pred_variables[[paste0("Pred_",i)]] <- fcast
}

########################################################################################################
#######################Modelling Pre - Posts#########################################################
########################################################################################################


models <- c("CameTodayScheduledEarlier","CameTodayScheduledLater" )

# RMSE = c()
for (i in models){
  train_dynamic <- train[,c(i, train_charact)]
  fit <- train(
    as.formula(paste(i,"~ ."))
    ,data = train_dynamic
    ,method = "ranger"
    ,trControl = trainControl(method="cv", number = 5, allowParallel = TRUE, verbose = TRUE)
    #,tuneGrid = expand.grid(mtry = c(3,4,5))
    ,importance = 'impurity'
    ,min.node.size = 5
    ,num.trees = 30
  )
  #accuracy
  fcast = predict(fit, pred_variables)
  pred_variables[[paste0("Pred_",i)]] <- fcast

}


# #######################Modelling Pre - Posts- Bucketed#########################################################
# 
# models <- colnames(plus3_dpt_data)[24:30]
# 
# RMSE = c()
# j=24
# for (i in models){
#   train_dynamic <- train[,c(3,5,j,31:310)]
#   fit <- train(
#     as.formula(paste(i,"~ ."))
#     ,data = train_dynamic
#     ,method = "ranger"
#     ,trControl = trainControl(method="cv", number = 5, allowParallel = TRUE, verbose = TRUE)
#     #,tuneGrid = expand.grid(mtry = c(3,4,5))
#     ,importance = 'impurity'
#     ,min.node.size = 5
#   )
#   #varImp(fit)
#   #varinfo = as.data.frame(varImp(fit)[1])
#   #accuracy
#   fcast = predict(fit, test)
#   test[[paste0("Pred_",i)]] <- fcast
#   j = j+1
# }

#Final Model
predict <- c("Date","Bucket","active","Pred_Deleted","Pred_Added","Pred_S0D0SB",
             "Pred_LTE_minus2_Early","Pred_minus1_Early","Pred_On_Time" ,"Pred_Plus1_Delay",
             "Pred_Plus2_Delay","Pred_Plus3_Delay","Pred_Plus4_Delay" ,"Pred_GTE_Plus5_Delay",
             "Pred_Early","Pred_Delay",
             "Pred_CameTodayScheduledEarlier","Pred_CameTodayScheduledLater")

Final <- pred_variables[, predict] 
Final <- Final[with(Final, order(Date, Bucket)), ]
names(Final)

###############################################################################################
##########PRE_POST from EARLY_DELAY############################################################
# Final$POST <- (lag(Final$Pred_LTE_minus2_Early,1)
#                + lag(Final$Pred_minus1_Early,2))
# Final$PRE <- (lead(Final$Pred_Plus1_Delay,1)
#                + lead(Final$Pred_Plus2_Delay,2)
#                + lead(Final$Pred_Plus3_Delay,3)
#                + lead(Final$Pred_Plus4_Delay,4)
#                + lead(Final$Pred_GTE_Plus5_Delay,5))
# Final[is.na(Final$PRE), "PRE"] <- mean(Final$PRE, na.rm = TRUE)
# Final[is.na(Final$POST), "POST"] <- mean(Final$POST, na.rm = TRUE)
# rms <- rmse(Final$PRE, test$CameTodayScheduledEarlier)
# rms
# rms <- rmse(Final$POST, test$CameTodayScheduledLater)
# rms


####################FINAL ANALYSIS#############################################################
Final$Pred_Deviation <-     round(
  Final$Pred_Added
  - Final$Pred_Deleted
  #PRE_POST Modelled
  + Final$Pred_CameTodayScheduledEarlier
  + Final$Pred_CameTodayScheduledLater
  #PRE_POST Bucketwise Modelled
  # + Final$Pred_LTE_minus2_Pre
  # + Final$Pred_Pre_minus1_Pre
  # + Final$Pred_Plus1_Post
  # + Final$Pred_Plus2_Post
  # + Final$Pred_Plus3_Post
  # + Final$Pred_Plus4_Post
  # + Final$Pred_GTE_Plus5_Post
  #PRE_POST from DELAY-EARLY
  # + Final$POST
  # + Final$PRE
  #EARLY_DELAY
  - Final$Pred_Early
  - Final$Pred_Delay
  #EARLY_DELAY Bucketwise
  # - Final$Pred_LTE_minus2_Early
  # - Final$Pred_minus1_Early
  # - Final$Pred_Plus1_Delay
  # - Final$Pred_Plus2_Delay
  # - Final$Pred_Plus3_Delay
  # - Final$Pred_Plus4_Delay
  # - Final$Pred_GTE_Plus5_Delay
)
Final$Pred_Actuals <- Final$active + Final$Pred_Deviation 

write.csv(Final, paste0("Actual_Predicted_Day+",day,"_v4.csv"))

####################### Transit Time Prediction#####################################################
#######################################################################################################
models <- c("Transit_Bucket_2B","Transit_Bucket_3B","Transit_Bucket_4B","Transit_Bucket_5B",          
            "Transit_Bucket_6B","Transit_Bucket_GT7B")

for (i in models){
  print(i)
  train_dynamic <- train[,c(i,"active", train_charact)]
  fit <- train(
    as.formula(paste(i,"~ ."))
    ,data = train_dynamic
    ,method = "ranger"
    ,trControl = trainControl(method="cv", number = 2, allowParallel = TRUE, verbose = TRUE)
    #,tuneGrid = expand.grid(mtry = 4)
    ,importance = 'impurity'
    ,min.node.size = 5
  )

  #accuracy
  fcast = predict(fit, pred_variables)
  pred_variables[[paste0("Pred_",i)]] <- fcast
  Final[[paste0("Pred_",i)]] <- fcast

}

#write.csv(pred_variables, "Predicted_Day.csv")


##########################################################################################################################
############################Transit time naive probability analysis
models <- c("Transit_Bucket_2B","Transit_Bucket_3B","Transit_Bucket_4B","Transit_Bucket_5B",          
            "Transit_Bucket_6B","Transit_Bucket_GT7B")
train_dynamic <- train[,c("Date","Bucket","weekday",models)]
train_dynamic$sum <- rowSums(train_dynamic[,models]) 
train_dynamic$Prob_TB_2B <- train_dynamic$Transit_Bucket_2B/train_dynamic$sum
train_dynamic$Prob_TB_3B <- train_dynamic$Transit_Bucket_3B/train_dynamic$sum
train_dynamic$Prob_TB_4B <- train_dynamic$Transit_Bucket_4B/train_dynamic$sum
train_dynamic$Prob_TB_5B <- train_dynamic$Transit_Bucket_5B/train_dynamic$sum
train_dynamic$Prob_TB_6B <- train_dynamic$Transit_Bucket_6B/train_dynamic$sum
train_dynamic$Prob_TB_GT7B <- train_dynamic$Transit_Bucket_GT7B/train_dynamic$sum


# train_bucket_means <- train_dynamic %>% group_by(Bucket) %>% summarise(Pred_Prob_TB_2B = mean(Prob_TB_2B, na.rm=TRUE),
#                                                                Pred_Prob_TB_3B = mean(Prob_TB_3B, na.rm=TRUE),
#                                                                Pred_Prob_TB_4B = mean(Prob_TB_4B, na.rm=TRUE),
#                                                                Pred_Prob_TB_5B = mean(Prob_TB_5B, na.rm=TRUE),
#                                                                Pred_Prob_TB_6B = mean(Prob_TB_6B, na.rm=TRUE),
#                                                                Pred_Prob_TB_GT7B = mean(Prob_TB_GT7B, na.rm=TRUE))
train_Weekday_means <- train_dynamic %>% group_by(weekday) %>% summarise(Pred_Prob_TB_2B = mean(Prob_TB_2B, na.rm=TRUE),
                                                                       Pred_Prob_TB_3B = mean(Prob_TB_3B, na.rm=TRUE),
                                                                       Pred_Prob_TB_4B = mean(Prob_TB_4B, na.rm=TRUE),
                                                                       Pred_Prob_TB_5B = mean(Prob_TB_5B, na.rm=TRUE),
                                                                       Pred_Prob_TB_6B = mean(Prob_TB_6B, na.rm=TRUE),
                                                                       Pred_Prob_TB_GT7B = mean(Prob_TB_GT7B, na.rm=TRUE))
# Weekday_bucket_means <- train_dynamic %>% group_by(weekday,Bucket) %>% summarise(Pred_Prob_TB_2B = mean(Prob_TB_2B, na.rm=TRUE),
#                                                                          Pred_Prob_TB_3B = mean(Prob_TB_3B, na.rm=TRUE),
#                                                                          Pred_Prob_TB_4B = mean(Prob_TB_4B, na.rm=TRUE),
#                                                                          Pred_Prob_TB_5B = mean(Prob_TB_5B, na.rm=TRUE),
#                                                                          Pred_Prob_TB_6B = mean(Prob_TB_6B, na.rm=TRUE),
#                                                                          Pred_Prob_TB_GT7B = mean(Prob_TB_GT7B, na.rm=TRUE))

pred_variables <- merge(pred_variables, train_bucket_means)
#pred_variables <- merge(pred_variables, train_Weekday_means)
#pred_variables <- merge(pred_variables, Weekday_bucket_means)

pred_variables<- pred_variables[with(pred_variables, order(Date,Bucket)), ]
write.csv(pred_variables, "Predicted_Day.csv")








