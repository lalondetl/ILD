

#' Survival and Recurrent Events Analysis Data Formatting for Intensive Longitudinal EMA Data
#'
#' This function prepares the data for analysis of effects on time to next use for intensive longitudinal EMA data.  
#' @param directory The directory where the EMA dataframe is available.    
#' @keywords Intensive Longitudinal Data Survival EMA
#' @export 
#' @examples
#' ILDREAFormat()



ILDREAFormat = function(directory){


setwd('directory/')

# READ IN THE RAW DATA #
EMAData = read.csv('EMADataReduced_16_17.csv')


##########################################################################
## CREATE SINGLE TIMESTAMPS FOR TIMEOFUSE, TIMEOFPROMPT, TIMEOFRESPONSE ##
##########################################################################


# TIMEOFUSE #

# DAY NUMBER FROM WhenDay #
# gsub(".* ","",EMAData$WhenDay[1])

# MONTH ABBREVIATION FROM WhenDay #
# sub(".* (.*) .*","\\1",EMAData$WhenDay[1])

# MONTH FROM Date #
# gsub("/.*","",EMAData$Date[1])

# YEAR FROM Date #
# gsub(".*/","",EMAData$Date[1])


EMAData$TimeOfUse = as.POSIXct(paste(gsub(".*/","",EMAData$Date), sub(".* (.*) .*","\\1",EMAData$WhenDay), gsub(".* ","",EMAData$WhenDay), EMAData$WhenTime), format = "%y %b %d %I:%M %p")



##########################################################################


# TIMEOFPROMPT #
EMAData$TimeOfPrompt = as.POSIXct(paste(EMAData$Date, EMAData$PromptTime), format = "%m/%d/%y %H:%M:%S")


##########################################################################


# TIMEOFRESPONSE #

# HOURS: gsub(":.*","",EMAData$ResponseLapse[1]) #

# MINUTES: sub(".*:(.*):.*","\\1",EMAData$ResponseLapse[1]) #

# SECONDS: gsub(".*:","",EMAData$ResponseLapse[1]) #


EMAData$TimeOfResponse = EMAData$TimeOfPrompt + as.numeric(gsub(".*:","",EMAData$ResponseLapse)) + 60*as.numeric(sub(".*:(.*):.*","\\1",EMAData$ResponseLapse)) + 60*60*as.numeric(gsub(":.*","",EMAData$ResponseLapse))


##########################################################################


## SORT BY ID, PROMPT TIME ##
EMAData = EMAData[order(EMAData$ID,EMAData$TimeOfPrompt),]


## CREATE MINIMUM PROMPT TIME FOR ALL SUBJECTS, FOR USE IN CALCULATING HOURS ##
mins = aggregate(TimeOfPrompt~ID,EMAData,function(x)min(x,na.rm=TRUE))
colnames(mins) = c("ID","MinPromptTime")
EMAData = merge(EMAData,mins,by.x="ID",by.y="ID")



## (1) ELIMINATE NA'S IN BOTH USE AND RESPONSE TIME STAMPS ##

# LEAVES 1049 TIME STAMPS #
TimeOfUseData = EMAData[!is.na(EMAData$TimeOfUse),]

# LEAVES 2148 TIME STAMPS #
TimeOfResponseData = EMAData[!is.na(EMAData$TimeOfResponse),]




## (2) CREATE HOURS TO USE, HOURS TO RESPONSE USING FIRST PROMPT TIME ##
##	STUDY BEGINS AT FIRST PROMPT!!!		##


## CREATE AN INDICATOR FOR REPORTING USE IN THE FUTURE ##
TimeOfUseData$futureTime = rep(0,nrow(TimeOfUseData))
for(i in 2:nrow(TimeOfUseData))
{
	# IF TIME OF USE IS LATER THAN TIME OF RESPONSE #
	if(difftime(TimeOfUseData$TimeOfUse[i],TimeOfUseData$TimeOfResponse[i]) > 0)
	{
		# MARK AS FUTURE #
		TimeOfUseData$futureTime[i] = 1
	}
}

# REMOVE FUTURE TIMES OF USE #
# LEAVES 1023 TIMES OF USE #
TimeOfUseData = TimeOfUseData[(which(TimeOfUseData$futureTime==0)),]


## CREATE AN INDICATOR FOR AN ERROR IN REPORTING TIME OF USE ##
TimeOfUseData$wrongTime = rep(0,nrow(TimeOfUseData))
for(i in 2:nrow(TimeOfUseData))
{
	# IF EARLIER THAN PREVIOUS TIME OF USE AND SAME PARTICIPANT #
	if((difftime(TimeOfUseData$TimeOfUse[i],TimeOfUseData$TimeOfUse[i-1]) < 0) & (TimeOfUseData$ID[i] == TimeOfUseData$ID[i-1]))
	{
		# MARK AS WRONG #
		TimeOfUseData$wrongTime[i] = 1
	}
}

# FOR NOW, REMOVE WRONG TIMES OF USE #
# LEAVES 988 TIMES OF USE #
TimeOfUseData = TimeOfUseData[(which(TimeOfUseData$wrongTime==0)),]



# HOURS TO USE #
TimeOfUseData$HoursToUse = difftime(TimeOfUseData$TimeOfUse,TimeOfUseData$MinPromptTime,units="hours")

# REMOVE USE PRIOR TO START OF STUDY #
# LEAVES 945 TIMES OF USE #
TimeOfUseData = TimeOfUseData[(which(TimeOfUseData$HoursToUse>0)),]


# KEEP ONLY VARIABLES OF INTEREST #
UseData = data.frame(ID=TimeOfUseData$ID, Event=TimeOfUseData$Used, Hours=TimeOfUseData$HoursToUse)
UseData$Event = rep(1,nrow(UseData))





# HOURS TO RESPONSE #
# NOTE: THESE ARE AUTOMATED AND NEED NO CORRECTIONS #
TimeOfResponseData$HoursToResponse = difftime(TimeOfResponseData$TimeOfResponse,TimeOfResponseData$MinPromptTime,units="hours")



## (3) PASTE INTO ONE DATA SET ##


# KEEP ONLY VARIABLES OF INTEREST #
ResponseData = data.frame(ID=TimeOfResponseData$ID, Hours=TimeOfResponseData$HoursToResponse, Motivation=TimeOfResponseData$Motivation, Anxiety=TimeOfResponseData$Anxiety, Mood=TimeOfResponseData$Mood, Frequency=TimeOfResponseData$Frequency, Others=TimeOfResponseData$Others, Drinks=TimeOfResponseData$Drinks, Time=TimeOfResponseData$Time, Studying=TimeOfResponseData$Studying, Exercising=TimeOfResponseData$Exercising, Craving=TimeOfResponseData$Craving)




# ADD VARIABLES TO MATCH DATA SETS #
UseData$Motivation = rep(NA,nrow(UseData))
UseData$Anxiety = rep(NA,nrow(UseData))
UseData$Mood = rep(NA,nrow(UseData))
UseData$Frequency = rep(NA,nrow(UseData))
UseData$Others = rep(NA,nrow(UseData))
UseData$Drinks = rep(NA,nrow(UseData))
UseData$Time = rep(NA,nrow(UseData))
UseData$Studying = rep(NA,nrow(UseData))
UseData$Exercising = rep(NA,nrow(UseData))
UseData$Craving = rep(NA,nrow(UseData))

ResponseData$Event = rep(0,nrow(ResponseData))


# REORDER COLUMNS FOR RBIND #
ResponseData = ResponseData[,c(1,13,2,3,4,5,6,7,8,9,10,11,12)]


SurvivalData = rbind(UseData,ResponseData)




## (4) SORT BY HOURS (WITHIN ID), COMBINING BOTH EVENTS AND NON-EVENTS ##
SurvivalData = SurvivalData[order(SurvivalData$ID,SurvivalData$Hours),]



## (5) FILL IN MISSING TVC VALUES WITH PRIOR (EXCEPT FOR FIRST) ##

for(i in 2:nrow(SurvivalData))
{
	# IF EVENT AND SAME PARTICIPANT #
	if((SurvivalData$Event[i] == 1) & (SurvivalData$ID[i] == SurvivalData$ID[i-1]))
	{
		# INHERIT PRIOR TVC VALUES #
		SurvivalData$Motivation[i] = SurvivalData$Motivation[i-1]
		SurvivalData$Anxiety[i] = SurvivalData$Anxiety[i-1]
		SurvivalData$Mood[i] = SurvivalData$Mood[i-1]
		SurvivalData$Frequency[i] = SurvivalData$Frequency[i-1]
		SurvivalData$Others[i] = SurvivalData$Others[i-1]
		SurvivalData$Drinks[i] = SurvivalData$Drinks[i-1]
		SurvivalData$Time[i] = SurvivalData$Time[i-1]
		SurvivalData$Studying[i] = SurvivalData$Studying[i-1]
		SurvivalData$Exercising[i] = SurvivalData$Exercising[i-1]
		SurvivalData$Craving[i] = SurvivalData$Craving[i-1]
	}
}




## (6) DEFINE CP-TIME ##

SurvivalData$CPStart = rep(0,nrow(SurvivalData))
for(i in 2:nrow(SurvivalData))
{
	# IF SAME SUBJECT DEFINE, CP-START-TIME AS PREVIOUS TIME #
	if(SurvivalData$ID[i] == SurvivalData$ID[i-1])
	{
		SurvivalData$CPStart[i] = SurvivalData$Hours[i-1]
	}
}

# CP-END-TIME IS HOURS #
SurvivalData$CPEnd = SurvivalData$Hours





## (7) DEFINE GT-TIME ##

# GT-START-TIME IS ALWAYS ZERO #
SurvivalData$GTStart = rep(0,nrow(SurvivalData))

# GT-END-TIME IS DIFFERENCE BETWEEN CP-TIMES #
SurvivalData$GTEnd = SurvivalData$CPEnd - SurvivalData$CPStart





## (8) IMPORT TIME-INDEPENDENT COVARIATES: GENDER, RMPI ##

# READ IN THE BASELINE DATA #
setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/EMA/2018/Survival Analysis/Data/')
library(foreign)
EMABaseline = read.spss(file='EMA_NIH_Baseline_Data 2.1.18.sav', to.data.frame=TRUE)


# KEEP VARIABLES OF INTEREST: RMPI_total, Gender #
BaselineData = data.frame(ID=EMABaseline$ID, RMPI=EMABaseline$RMPI_total, Gender=EMABaseline$Gender)



# MERGE WITH EMA DATA #

# MAKE ID NUMERIC #
id = levels(BaselineData$ID)[BaselineData$ID]
id = as.numeric(id)
BaselineData$ID = id

# BRING IN ID BRIDGE FILE #
IDData = read.csv('IDData.csv')

# ADD EMAID TO SURVIVAL DATA: ID IN SURVIVAL FILE IS EQUIVALENT TO LDID IN BRIDGE FILE #
SurvivalMatched = merge(SurvivalData,IDData,by.x='ID',by.y='LDID')

# EMAID IS EQUIVALENT TO BASELINE ID #
SurvivalData = merge(SurvivalMatched,BaselineData,by.x='EMAID',by.y='ID')





## (9) CREATE STRATUM FOR EVENT COUNT (CUMSUM) ##

# SORT BY ID AND TIME #
SurvivalData = SurvivalData[order(SurvivalData$ID,SurvivalData$CPStart),]


# ACCUMULATE BY TIME #
SurvivalData$Stratum = ave(SurvivalData$Event, SurvivalData$ID, FUN=cumsum)




# SAVE FINAL DATA #
write.csv(SurvivalData, file='SurvivalData.csv', quote=FALSE, row.names=FALSE)








}




