

#' Time Since Last Use Analysis for Intensive Longitudinal EMA Data
#'
#' This function produces analysis of time since last use for intensive longitudinal EMA data.  
#' @param directory The directory where the EMA dataframe is available.    
#' @keywords Intensive Longitudinal Data Time EMA
#' @export 
#' @examples
#' TimeSinceUse()



TimeSinceUse = function(directory){


setwd('directory/')

# READ IN THE RAW DATA #
EMAData = read.csv('EMADataLongFormat_16_17.csv')



## CREATE SINGLE TIMESTAMPS FOR TIMEOFUSE, TIMEOFPROMPT, TIMEOFRESPONSE ##

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




# TIMEOFPROMPT #
EMAData$TimeOfPrompt = as.POSIXct(paste(EMAData$Date, EMAData$PromptTime), format = "%m/%d/%y %H:%M:%S")




# TIMEOFRESPONSE #

# HOURS: gsub(":.*","",EMAData$ResponseLapse[1]) #

# MINUTES: sub(".*:(.*):.*","\\1",EMAData$ResponseLapse[1]) #

# SECONDS: gsub(".*:","",EMAData$ResponseLapse[1]) #


EMAData$TimeOfResponse = EMAData$TimeOfPrompt + as.numeric(gsub(".*:","",EMAData$ResponseLapse)) + 60*as.numeric(sub(".*:(.*):.*","\\1",EMAData$ResponseLapse)) + 60*60*as.numeric(gsub(":.*","",EMAData$ResponseLapse))





## SORT BY ID, TIMEOFPROMPT #
EMAData = EMAData[order(EMAData$ID,EMAData$TimeOfPrompt),]



# UPDATE TimeOfUse FOR MBD #

for(i in 1:nrow(EMAData))
{
	# IF RESPONDED AND NO USE #
	if(EMAData$Responded[i] == 1 & !is.na(EMAData$Used[i])){
	if(EMAData$Used[i] == 0)
	{
		# IF STILL THE SAME SUBJECT (OK B/C i=1 SHOWS USE) #
		if(EMAData$ID[i] == EMAData$ID[i-1])
		{
			# CARRY OVER PREVIOUS USE TIME #
			EMAData$TimeOfUse[i] = EMAData$TimeOfUse[i-1]
		}
	}
	}

	# IF NO RESPONSE, CARRY OVER PREVIOUS TIME FOR CALCULCATION (RESPONSE TIME WILL BE N/A) #
	if(EMAData$Responded[i] == 0 | is.na(EMAData$Used[i]))
	{
		# IF STILL THE SAME SUBJECT (OK B/C i=1 SHOWS USE) #
		if(EMAData$ID[i] == EMAData$ID[i-1])
		{
			# CARRY OVER PREVIOUS USE TIME #
			EMAData$TimeOfUse[i] = EMAData$TimeOfUse[i-1]
		}
	}
}



## CREATE TIMESINCELASTUSE #

EMAData$TSLU = as.numeric(difftime(EMAData$TimeOfResponse,EMAData$TimeOfUse,units="mins"))

# Less0 = EMAData[which(EMAData$TSLU<0),]


## QUICK DESCRIPTIVES OF TSLU ##

EMADataTSLU = EMAData[which(EMAData$TSLU>=0),]

fivenum(EMADataTSLU$TSLU)
mean(EMADataTSLU$TSLU,na.rm=TRUE)
var(EMADataTSLU$TSLU,na.rm=TRUE)


setwd("/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/EMA/NIH Analysis Fall 17/Summaries/")

pdf("FullHistogram.pdf")
hist(EMADataTSLU$TSLU)
dev.off()

# KEEP USE WITHIN 48 HOURS: REMOVES 98 OBSERVATIONS #
TSLU48 = EMADataTSLU[which(EMADataTSLU$TSLU<2880),]$TSLU

pdf("48Histogram.pdf")
hist(TSLU48)
dev.off()


# KEEP USE WITHIN 24 HOURS: REMOVES 343 OBSERVATIONS #
TSLU24 = EMADataTSLU[which(EMADataTSLU$TSLU<1440),]$TSLU

pdf("24Histogram.pdf")
hist(TSLU24)
dev.off()


}

