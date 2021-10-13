
###################################################
# Biomedical Data Science Day 2020
# Using R to Process and Analyze Accelerometer Data
# Author: David Aaby
# Updated: January 27 2020
###################################################


# NHANES 2003-2004 Physical Activity Monitor
# DESCRIPTION OF VARIABLES
# https://wwwn.cdc.gov/nchs/nhanes/2003-2004/PAXRAW_C.htm

# PAXSTAT: Component status code with PAXSTAT=1 for records with data that are deemed reliable. 
#   A PAXSTAT=2 was used to code records that had some questionable data; 
#   analysts may wish to examine these records more closely.
# 
# PAXDAY: Day of the week; PAXDAY=1 for Sunday, 2 for Monday and so forth.
# 
# PAXN: Sequential observation number in minutes as recorded by the monitor device. 
#   The range begins with minute 1 on Day 1 (PAXN=1) and ends with the last minute of day 7 
#   of monitor wear (PAXN=10080). Each day of wear produces 1440 individual minute records. 
#   The PAXN values for Day 1 range from 1 to 1440; Day 2 PAXN range from 1441-2880, and so forth.
# 
# PAXHOUR-hour of day the intensity data were recorded in military time or 24 hour clock.
# 
# PAXMINUT- minute value associated with a particular hour (PAXHOUR). The minute value shown is 
#   the start of the minute. For example, for the time 1201 hours, the start of the minute occurs 
#   at 1200 hours and PAXMINUT would be 00.
# 
# PAXINTEN is the intensity value recorded by the device. Each minute has an intensity value.
# 
# PAXCAL- Denotes whether the monitor was in calibration when it was returned by the subject. 
#   The data for monitors that were out of calibration (PMACAL=2) may be less reliable.




##################
# load libraries #
##################

# you must first install the package if it is not already installed #
install.packages("accelerometry")

# load package into R #
library(accelerometry)


#########################
# set working directory #
#########################

setwd("R:/PrevMed/Projects/RelativeIntensity/Presentations/Biomed Data Science 2020")


#############
# load data #
#############

# this file contains 100 subjects from the NHANES 2003-2004 Physical Activity Monitor  #
df = read.csv("data/NHANES_accel_100.csv", header=T)


# quick look at data #
head(df)
dim(df)

length(unique(df$seqn))



##########################
# create one file per ID #
##########################

# NOTE: it will be easier for us to process the data if every participant has a separate accelerometer file

ids = unique(df$seqn)


# create one file per ID #
for(i in 1:length(ids)) {
  print(c(i,ids[i]))
  oneperid = df[which(df$seqn == ids[i]),]
  write.csv(oneperid, file=paste("data/one_per_id/ID_", oneperid$seqn[1], ".csv", sep=""), row.names = FALSE)
}



#########################################################################
# example 1: process and analyze accelerometer data for one participant #
#########################################################################

# read in data #
unidata = read.csv("data/one_per_id/ID_21005.csv", header=T)



# plot all counts for one participant #
plot(unidata$paxinten, 
     main=paste("Counts Per Minute for ID ", unidata$seqn[1], sep=""),
     xlab="Index minute",
     ylab="Count per minute (cpm)")

# we could show how often the subject reaches the 2020 cpm threshold for MVPA #
abline(h=2020, col="red", lty=2, lwd=2)

# we could add lines to show where each new day begins #
days = unidata[!duplicated(unidata[, "paxday"], fromLast=TRUE),]
abline(v=days$paxn, col="black", lty=2, lwd=2)



# process accelerometer data for one participant - default parameters #

summary.daily.i <- process_uni(counts=unidata$paxinten, start_day = unidata$paxday[1])
summary.daily.i





# process accelerometer data for one participant - slightly custom parameters #

summary.daily.i <- process_uni(counts=unidata$paxinten,
                                     steps = NULL,   # default, we are not using steps
                                     nci_methods = FALSE,  # default
                                     start_day = unidata$paxday[1],
                                     start_date = NULL,  # NHANES data example does not have dates
                                     #start.time = strtime,
                                     id = unidata$seqn[1],
                                     brevity = 2,   #default is 1, we want more info
                                     #hourly_var = "cpm",
                                     #hourly_wearmin = 0,
                                     valid_days = 1,     # we will remove subjects with < 4 valid days after we run the function
                                     valid_wk_days = 0, # default
                                     valid_we_days = 0, # default
                                     
                                     int_cuts = c(100,760,2020,5999),    # default values
                                     cpm_nci = FALSE ,  # default
                                     days_distinct = FALSE, # default
                                     nonwear_window = 60,   # default
                                     nonwear_tol = 2,  # default is 0
                                     nonwear_tol_upper = 99,
                                     nonwear_nci = FALSE, # default
                                     weartime_minimum = 600,   # default
                                     weartime_maximum = 1440,  # default
                                     active_bout_length = 10,  # default
                                     active_bout_tol = 2,   # default is 0
                                     mvpa_bout_tol_lower = 0, # default
                                     vig_bout_tol_lower = 0, # default
                                     active_bout_nci = FALSE, # default
                                     sed_bout_tol = 0, # default
                                     sed_bout_tol_maximum = 759,  # int.cuts[2] - 1
                                     artifact_thresh = 25000,    # default
                                     artifact_action = 3,        # if 3, replace artifacts with average of neighboring count values
                                     weekday_weekend = FALSE,  # default
                                     return_form = "daily")  # get per-day summary for each day of wear


summary.daily.i

# what does the output give us?
  # day = what day are we talking about? sunday=1, monday=2, etc.
  # valid_day : indicator for whether the day is valid or not
  # valid_min : total valid minutes per day
  # counts: total counts each day
  # sed_min : total minutes of sedentary time per day
  # mvpa_min : total minutes of mvpa per day



########################################################################################################
# example 2: read in accelerometer data for many participants and combine summary into single data set #
########################################################################################################

accel.filenames = list.files("data/one_per_id")
accel.filenames = paste("data/one_per_id/", accel.filenames, sep="")

summary.daily = NULL

for(i in 1:length(accel.filenames)) {

  file = accel.filenames[i]
  
  unidata <- read.csv(file, header=T)
  
  print(c(i, unidata$seqn[1]))
  
  
  summary.daily.i <- process_uni(counts=unidata$paxinten,
                                 steps = NULL,   # default, we are not using steps
                                 nci_methods = FALSE,  # default
                                 start_day = unidata$paxday[1],
                                 start_date = NULL,  # NHANES data example does not have dates
                                 #start.time = strtime,
                                 id = unidata$seqn[1],
                                 brevity = 2,   #default is 1, we want more info
                                 #hourly_var = "cpm",
                                 #hourly_wearmin = 0,
                                 valid_days = 1,     # we will remove subjects with < 4 valid days after we run the function
                                 valid_wk_days = 0, # default
                                 valid_we_days = 0, # default
                                 
                                 int_cuts = c(100,760,2020,5999),    # default values
                                 cpm_nci = FALSE ,  # default
                                 days_distinct = FALSE, # default
                                 nonwear_window = 60,   # default
                                 nonwear_tol = 2,  # default is 0
                                 nonwear_tol_upper = 99,
                                 nonwear_nci = FALSE, # default
                                 weartime_minimum = 600,   # default
                                 weartime_maximum = 1440,  # default
                                 active_bout_length = 10,  # default
                                 active_bout_tol = 2,   # default is 0
                                 mvpa_bout_tol_lower = 0, # default
                                 vig_bout_tol_lower = 0, # default
                                 active_bout_nci = FALSE, # default
                                 sed_bout_tol = 0, # default
                                 sed_bout_tol_maximum = 759,  # int.cuts[2] - 1
                                 artifact_thresh = 25000,    # default
                                 artifact_action = 3,        # if 3, replace artifacts with average of neighboring count values
                                 weekday_weekend = FALSE,  # default
                                 return_form = "daily")  # get per-day summary for each day of wear
    
    summary.daily.i = data.frame(summary.daily.i)
    
    summary.daily = rbind(summary.daily, summary.daily.i)

}


summary.daily[which(summary.daily$id==21006),]




####################################
# Example 3: how to summarize data #
####################################

# Let's compare minutes of MVPA in males and females #


# merge in demographics data #
demo = read.csv(file="data/NHANES_demo_subset.csv", header = TRUE)

summary.daily = merge(summary.daily, demo, by = "id", all.x=TRUE)

# keep only those IDs with 4 or more valid days #
valid.days = aggregate(valid_day ~ id, data = summary.daily, FUN = sum)
four.valid.days = valid.days$id[which(valid.days$valid_day >= 4)]

sum.daily.4vd = summary.daily[summary.daily$id %in% four.valid.days,] 


# keep only the valid days #
sum.daily.4vd = sum.daily.4vd[which(sum.daily.4vd$valid_day==1),]

mvpa.avg = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=mean)
mvpa.avg = merge(mvpa.avg, demo, by = "id", all.x=TRUE)

mvpa.sum = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=sum)
mvpa.sum = merge(mvpa.sum, demo, by = "id", all.x=TRUE)

mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==1)])
mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==0)])

mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==1)])
mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==0)])



#########################################################################
# example 4: use individualized intensity cutpoints and re-run analysis #
#########################################################################

filenames.ex4 = data.frame(id = demo$id,
                             cutpt = demo$cutpt,
                             accel.filenames = accel.filenames,
                             stringsAsFactors = FALSE)


summary.daily.ex4 = NULL

for(i in 1:nrow(filenames.ex4)) {
  
  file = filenames.ex4$accel.filenames[i]
  
  unidata <- read.csv(file, header=T)
  
  mvpa.cutpt = demo$cutpt[i]
  
  print(c(i, unidata$seqn[1]))
  
  
  summary.daily.i <- process_uni(counts=unidata$paxinten,
                                 steps = NULL,   # default, we are not using steps
                                 nci_methods = FALSE,  # default
                                 start_day = unidata$paxday[1],
                                 start_date = NULL,  # NHANES data example does not have dates
                                 #start.time = strtime,
                                 id = unidata$seqn[1],
                                 brevity = 2,   #default is 1, we want more info
                                 #hourly_var = "cpm",
                                 #hourly_wearmin = 0,
                                 valid_days = 1,     # we will remove subjects with < 4 valid days after we run the function
                                 valid_wk_days = 0, # default
                                 valid_we_days = 0, # default
                                 
                                 int_cuts = c(1,2,mvpa.cutpt,16000),    # NEW CUTSTOM CUTPOINTS
                                 cpm_nci = FALSE ,  # default
                                 days_distinct = FALSE, # default
                                 nonwear_window = 60,   # default
                                 nonwear_tol = 2,  # default is 0
                                 nonwear_tol_upper = 99,
                                 nonwear_nci = FALSE, # default
                                 weartime_minimum = 600,   # default
                                 weartime_maximum = 1440,  # default
                                 active_bout_length = 10,  # default
                                 active_bout_tol = 2,   # default is 0
                                 mvpa_bout_tol_lower = 0, # default
                                 vig_bout_tol_lower = 0, # default
                                 active_bout_nci = FALSE, # default
                                 sed_bout_tol = 0, # default
                                 sed_bout_tol_maximum = 759,  # int.cuts[2] - 1
                                 artifact_thresh = 25000,    # default
                                 artifact_action = 3,        # if 3, replace artifacts with average of neighboring count values
                                 weekday_weekend = FALSE,  # default
                                 return_form = "daily")  # get per-day summary for each day of wear
  
  summary.daily.i = data.frame(summary.daily.i)
  
  summary.daily.ex4 = rbind(summary.daily.ex4, summary.daily.i)
  
}

summary.daily[which(summary.daily$id==21006),]
summary.daily.ex4[which(summary.daily.ex4$id==21006),]




summary.daily = merge(summary.daily.ex4, demo, by = "id", all.x=TRUE)

# keep only those IDs with 4 or more valid days #
valid.days = aggregate(valid_day ~ id, data = summary.daily, FUN = sum)
four.valid.days = valid.days$id[which(valid.days$valid_day >= 4)]

sum.daily.4vd = summary.daily[summary.daily$id %in% four.valid.days,] 


# keep only the valid days #
sum.daily.4vd = sum.daily.4vd[which(sum.daily.4vd$valid_day==1),]

mvpa.avg = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=mean)
mvpa.avg = merge(mvpa.avg, demo, by = "id", all.x=TRUE)

mvpa.sum = aggregate(mvpa_min ~ id, data=sum.daily.4vd, FUN=sum)
mvpa.sum = merge(mvpa.sum, demo, by = "id", all.x=TRUE)

mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==1)])
mean(mvpa.avg$mvpa_min[which(mvpa.avg$male==0)])

mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==1)])
mean(mvpa.sum$mvpa_min[which(mvpa.sum$male==0)])




