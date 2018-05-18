

#' Smoothed Plots for Intensive Longitudinal Functional Data
#'
#' This function produces loess smoothed plots for one school data of activity data.  
#' @param directory The directory where the functional dataframe is available.    
#' @keywords Intensive Longitudinal Data Functional Smooth
#' @export 
#' @examples
#' SmoothPlots()



SmoothPlots = function(directory){


#library(ggplot2)
#library(reshape2)
#library(readxl)



## READ IN DAY 1 DATA, 8:00 AM - 3:30 PM ONLY, WITH DEMOGRAPHICS ##
setwd('directory/')

Day1Complete = read.csv('Day1Complete.csv')

## MAKE PLOT GROUPS FACTORS ##



## FOR OUTPUT ##
setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

## READ IN SCHEDULE DATA ##
setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Data/Schedule Data/')
ScheduleW1 = read_excel("Continuous Schedule Week 1 School 1.xlsx")

## ADD PROPERLY FORMATTED START AND STOP TIMES ##

ScheduleW1$StartTime = as.POSIXct(paste("2016-11-09", gsub(".* ","",as.character(ScheduleW1$Start))), format = "%Y-%m-%d %H:%M:%S")

ScheduleW1$StopTime = as.POSIXct(paste("2016-11-09", gsub(".* ","",as.character(ScheduleW1$Stop))), format = "%Y-%m-%d %H:%M:%S")





############################
## TEACHER 1: 11 STUDENTS ##
############################

Teacher1 = Day1Complete[(which(Day1Complete$teacher=="1")),]


## FILTER BY TEACHER 1, DAY 1 ##
ScheduleW1T1D1 = ScheduleW1[(which((ScheduleW1$Teacher==1) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T1D1$StopTime[nrow(ScheduleW1T1D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T1D1 = as.data.frame(ScheduleW1T1D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T1D1$ActivityShort = ifelse((ScheduleW1T1D1$Activity %in% c("Dismissal","Math","Opening","Reading","Science","Writing")),"ClassTime",ScheduleW1T1D1$Activity)



## ALL SUBJECTS, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("AllSubjectsW1T1D1.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()








## ALL MALES AND FEMALES, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1Sex.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(Sex))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(Sex))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by Sex, Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## ALL WHITES AND NON-WHITES, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1Race.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(Race))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(Race))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by Race, Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1RawData.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##
## NO VARIATION ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1DIBELS2.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1DIBELS3.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##
## NO VARIATION ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T1D1AIMS.pdf")
ggplot(data =Teacher1, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T1D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 1, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





############################
## TEACHER 2: 14 STUDENTS ##
############################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #


Teacher2 = Day1Complete[(which(Day1Complete$teacher=="2")),]


## FILTER SCHEDULE BY TEACHER 2, DAY 1 ##
ScheduleW1T2D1 = ScheduleW1[(which((ScheduleW1$Teacher==2) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T2D1$StopTime[nrow(ScheduleW1T2D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T2D1 = as.data.frame(ScheduleW1T2D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T2D1$ActivityShort = ifelse((ScheduleW1T2D1$Activity %in% c("Dismissal","Math","Music","Opening","Reading","Science","Writing")),"ClassTime",ScheduleW1T2D1$Activity)


## REMOVE NA'S ##
Teacher2 = na.omit(Teacher2)




## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T2D1RawData.pdf")
ggplot(data =Teacher2, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T2D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 2, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T2D1DIBELS2.pdf")
ggplot(data =Teacher2, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T2D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 2, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T2D1DIBELS3.pdf")
ggplot(data =Teacher2, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T2D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 2, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T2D1AIMS.pdf")
ggplot(data =Teacher2, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T2D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 2, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()







############################
## TEACHER 3: 13 STUDENTS ##
############################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #


Teacher3 = Day1Complete[(which(Day1Complete$teacher=="3")),]


## FILTER SCHEDULE BY TEACHER 2, DAY 1 ##
ScheduleW1T3D1 = ScheduleW1[(which((ScheduleW1$Teacher==3) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T3D1$StopTime[nrow(ScheduleW1T3D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T3D1 = as.data.frame(ScheduleW1T3D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T3D1$ActivityShort = ifelse((ScheduleW1T3D1$Activity %in% c("Dismissal","Library","Literacy","Math","Music","Opening","Reading","Science")),"ClassTime",ScheduleW1T3D1$Activity)


## REMOVE NA'S ##
Teacher3 = na.omit(Teacher3)




## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T3D1RawData.pdf")
ggplot(data =Teacher3, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T3D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 3, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T3D1DIBELS2.pdf")
ggplot(data =Teacher3, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T3D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 3, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T3D1DIBELS3.pdf")
ggplot(data =Teacher3, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T3D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 3, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T3D1AIMS.pdf")
ggplot(data =Teacher3, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T3D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 3, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






###########################
## TEACHER 4: 7 STUDENTS ##
###########################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #


Teacher4 = Day1Complete[(which(Day1Complete$teacher=="4")),]


## FILTER SCHEDULE BY TEACHER 2, DAY 1 ##
ScheduleW1T4D1 = ScheduleW1[(which((ScheduleW1$Teacher==4) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T4D1$StopTime[nrow(ScheduleW1T4D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T4D1 = as.data.frame(ScheduleW1T4D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T4D1$ActivityShort = ifelse((ScheduleW1T4D1$Activity %in% c("Dismissal","Math","Opening","Reading","Science","Specials","Writing")),"ClassTime",ScheduleW1T4D1$Activity)


## REMOVE NA'S ##
Teacher4 = na.omit(Teacher4)




## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T4D1RawData.pdf")
ggplot(data =Teacher4, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T4D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 4, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T4D1DIBELS2.pdf")
ggplot(data =Teacher4, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T4D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 4, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T4D1DIBELS3.pdf")
ggplot(data =Teacher4, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T4D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 4, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T4D1AIMS.pdf")
ggplot(data =Teacher4, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T4D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 4, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()









###########################
## TEACHER 6: 9 STUDENTS ##
###########################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #


Teacher6 = Day1Complete[(which(Day1Complete$teacher=="6")),]


## FILTER SCHEDULE BY TEACHER 6, DAY 1 ##
ScheduleW1T6D1 = ScheduleW1[(which((ScheduleW1$Teacher==6) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T6D1$StopTime[nrow(ScheduleW1T6D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T6D1 = as.data.frame(ScheduleW1T6D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T6D1$ActivityShort = ifelse((ScheduleW1T6D1$Activity %in% c("Art","Brain Gym","Dissmisal","Math","Music","Opening","Reading","Science")),"ClassTime",ScheduleW1T6D1$Activity)


## REMOVE NA'S ##
Teacher6 = na.omit(Teacher6)




## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T6D1RawData.pdf")
ggplot(data =Teacher6, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T6D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 6, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T6D1DIBELS2.pdf")
ggplot(data =Teacher6, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T6D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 6, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T6D1DIBELS3.pdf")
ggplot(data =Teacher6, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T6D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 6, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T6D1AIMS.pdf")
ggplot(data =Teacher6, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T6D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 6, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()









############################
## TEACHER 7: 14 STUDENTS ##
############################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #

Teacher7 = Day1Complete[(which(Day1Complete$teacher=="7")),]


## FILTER SCHEDULE BY TEACHER 7, DAY 1 ##
ScheduleW1T7D1 = ScheduleW1[(which((ScheduleW1$Teacher==7) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T7D1$StopTime[nrow(ScheduleW1T7D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T7D1 = as.data.frame(ScheduleW1T7D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T7D1$ActivityShort = ifelse((ScheduleW1T7D1$Activity %in% c("Art","Dismissal","Math","Opening","Reading","Library")),"ClassTime",ScheduleW1T7D1$Activity)


## REMOVE NA'S ##
Teacher7 = na.omit(Teacher7)




## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T7D1RawData.pdf")
ggplot(data =Teacher7, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T7D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 7, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T7D1DIBELS2.pdf")
ggplot(data =Teacher7, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T7D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 7, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T7D1DIBELS3.pdf")
ggplot(data =Teacher7, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T7D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 7, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T7D1AIMS.pdf")
ggplot(data =Teacher7, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T7D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 7, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()








############################
## TEACHER 8: 16 STUDENTS ##
############################

# RAW DATA AND ACADEMIC OUTCOMES ONLY #

Teacher8 = Day1Complete[(which(Day1Complete$teacher=="8")),]


## FILTER SCHEDULE BY TEACHER 8, DAY 1 ##
ScheduleW1T8D1 = ScheduleW1[(which((ScheduleW1$Teacher==8) & (ScheduleW1$Day==1))),]

# CORRECT DISMISSAL TIME GOING ALL NIGHT #
ScheduleW1T8D1$StopTime[nrow(ScheduleW1T8D1)] = "2016-11-09 15:30:00"

# MAKE TIBBLE A DATA FRAME #
ScheduleW1T8D1 = as.data.frame(ScheduleW1T8D1)


# COMBINE PERIODS OF INTEREST #
ScheduleW1T8D1$ActivityShort = ifelse((ScheduleW1T8D1$Activity %in% c("Dismissal","Handwriting","Literacy","Math","Opening","Random Thoughts/Rest Time/Movement","Whole group")),"ClassTime",ScheduleW1T8D1$Activity)


## REMOVE NA'S ##
Teacher8 = na.omit(Teacher8)



## ALL SUBJECTS, RAW DATA ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T8D1RawData.pdf")
ggplot(data =Teacher8, aes(x = CorrectTime, y =Axis1, colour=as.factor(awid))) + 
  geom_point(aes(color=as.factor(awid))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T8D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot of All Students, Teacher 8, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()




## DIBELS 2-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T8D1DIBELS2.pdf")
ggplot(data =Teacher8, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA162Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA162Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T8D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (2), Teacher 8, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()






## DIBELS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T8D1DIBELS3.pdf")
ggplot(data =Teacher8, aes(x = CorrectTime, y =Axis1, colour=as.factor(DiblesFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(DiblesFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T8D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by DIBELS (3), Teacher 8, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()





## AIMS 3-LEVEL, SMOOTHED USING SPAN = 0.05 ##

setwd('/Users/trent.lalonde/Documents/Research/Research Projects/Collaborative - Applied/Student Activity/5 Second Data/Summaries/')

pdf("W1T8D1AIMS.pdf")
ggplot(data =Teacher8, aes(x = CorrectTime, y =Axis1, colour=as.factor(AIMSFA163Level))) + 
  geom_smooth(span=0.05, method="loess", aes(group=as.factor(AIMSFA163Level))) +
  geom_rect(inherit.aes = FALSE, data = ScheduleW1T8D1, aes(xmin = as.character(StartTime), xmax = as.character(StopTime), ymin = -Inf, ymax = Inf, fill=as.factor(ActivityShort)), alpha = 0.4) +
  geom_hline(yintercept = c(8.5, 190.5,334.5)) +
  annotate("text",label = c("Sedentary","Light","Moderate","Vigorous"), x =c(300,300,300,300), y =c(-50, 100,250,400), size =3, colour = "red") +
  ggtitle("Plot by AIMS, Teacher 8, Day 1, School 1") + 
	scale_x_discrete(name="Time of Day", breaks=c("2016-11-09 08:00:01", "2016-11-09 10:00:01", "2016-11-09 12:00:01", "2016-11-09 14:00:01", "2016-11-09 15:30:00"), labels=c("8:00", "10:00", "12:00", "2:00", "3:30")) + 
	theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank())
dev.off()



}



