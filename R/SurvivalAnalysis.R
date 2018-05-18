

#' Survival and Recurrent Events Analysis for Intensive Longitudinal EMA Data
#'
#' This function produces analysis of effects on time to next use for intensive longitudinal EMA data.  
#' @param directory The directory where the EMA dataframe is available.    
#' @keywords Intensive Longitudinal Data Survival EMA
#' @export 
#' @examples
#' ILDREA()



ILDREA = function(directory){

setwd('directory/')

# READ IN THE RAW DATA #
SurvivalData = read.csv('SurvivalData.csv')



# FOR OUTPUT #
setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/EMA/2018/Survival Analysis/Summaries/')


##################
## DESCRIPTIVES ##
##################

library(KMsurv)
library(km.ci)
library(survival)


# SURVIVAL PLOTS #

# not quite right: not for recurrent events #
#SFunction = survfit(Surv(as.numeric(SurvivalData$CPEnd),SurvivalData$Event)~1, conf.type="log-log")

#summary(SFunction)

#pdf("SurvivalFunction.pdf")
#plot(SFunction,main="Survival to Dropout, in Years Since First 7th Grade",xlab="Years",ylab="Survival Probability")
#dev.off()


# HAZARD PLOTS #




# CONDITIONAL RECURRENT EVENT MODEL #


# CP-MODEL #



# GT-MODEL #


}




