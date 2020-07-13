library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)
theme_set(theme_bw())
#####################################################################################
############
#Data Import and Manipulation - Stage 1: Collecting and Summarizing CSV files from UV station
#####################################################################################
############
#####################################################################################
############
###### 20 SECOND ANALYSIS
########################################################################
system.time({
  AllData=data.frame(0,0,0,0,0)
  DoorSum<-data.frame(Date=NA, Year=NA, YMonth=NA, OpenDoorTime=NA, DoorActivatesUV=NA)
  DoorSum<-DoorSum[complete.cases(DoorSum),]
  UVSum<-data.frame(Date=NA,Year=NA, YMonth=NA,UVAinS=NA)
  UVSum<-UVSum[complete.cases(UVSum),]
  Test<-data.frame(Hour=0,Minute=0,Second=0)
  # To loop through directories
  dir.names <- list.dirs(full.names = FALSE, recursive = FALSE)
  for (room in dir.names){
    files <- list.files(file.path(room))
    #
    # # Looping through files
    for (filename in files) {
      OldData<-read.csv(file.path(room, filename), check.names=FALSE, header=FALSE)
      #OldData<-read.csv("ST1_180529000001.csv", check.names=FALSE, header=FALSE)
      colnames(OldData)<-c("Date","Time", "OutPmA", "OutPA", "Input Door Switch")
      OldData$OutPmA<-as.numeric(as.character(OldData$OutPmA))
      if (OldData$Date[1]=='Date'){OldData[1,]<-NA}
      if (OldData$Date[2]=='Date'){OldData[2,]<-NA}
      OldData<-OldData[complete.cases(OldData), ]
      #AllData<- OldData[complete.cases(OldData), ]
      AllData<- OldData[seq(1, nrow(OldData), 20), ]
      AllData$Room<-room
      AllData$Index<-seq.int(nrow(AllData))
      AllData$DoorOnOff<-gsub("[[:digit:]]", "", AllData$`Input Door Switch`)
      AllData$DoorOnOff<- gsub(' - ','',AllData$DoorOnOff)
      AllData$Door0_1<-as.numeric(gsub("[^[:digit:]]", "", AllData$`Input Door Switch`))
      AllData<-AllData[complete.cases(AllData), ]
      AllData$Date <- as.Date(AllData$Date, "%m/%d/%Y")
      AllData$DT <- strptime(paste(AllData$Date,AllData$Time), "%Y-%m-%d %H:%M:%S")
      AllData$Year <- substr(AllData$Date, 0, 4)
      AllData$YMonth <- substr(AllData$Date, 0, 7)
      AllData$DoorActivatesUV<-0
      for (i in AllData$Index) {
        if (i==1){
          AllData$TimeUVActivated=time_length(with(Test, hms(paste(Hour, Minute, Second, sep='
                                                                   '))),unit="seconds")
        }
        else {
          if (AllData$OutPmA[i]<=5){AllData$TimeUVActivated[i]=time_length(with(Test, hms(paste(Hour,
                                                                                                Minute, Second, sep=' '))), unit="second")}
          else {
            AllData$TimeUVActivated[i]=AllData$DT[i]-AllData$DT[i-1]+AllData$TimeUVActivated[i-1]
            if (i<nrow(AllData)){
              if (AllData$OutPmA[i+1]<=5){
                UVSum=rbind(UVSum,data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
                                             YMonth=AllData$YMonth[i], DT=AllData$DT[i], UVAinS=AllData$TimeUVActivated[i]))}
            }
          }
        }
        ##########################Check to see if door is closed (i.e. ON or 1)
        if (i==1){
          AllData$TimeDoorSwitchActivated=time_length(with(Test,
                                                           hms(paste(Hour, Minute, Second, sep=' '))), unit="seconds")}
        else{
          if (AllData$Door0_1[i]==1){
            ########################If it is closed, then TimeDoorSwitchActivated is set to 0 via the Test
            dataframe
            AllData$TimeDoorSwitchActivated[i]=time_length(with(Test,
                                                                hms(paste(Hour, Minute, Second, sep=' '))), unit="seconds")}
          ########################However, if the door is open (i.e. OFF or 0) then start the clock to see
          ########################how long the door is open for
          else {AllData$TimeDoorSwitchActivated[i]=AllData$DT[i]-AllData$DT[i-
                                                                              1]+AllData$TimeDoorSwitchActivated[i-1]
          ########Also, if the door is open, but the next time point the door is closed, and that you
          ########can check 2 time points into the future (with the assumption of 1 timepoint=20 sec
          ########which is not always the case if there is a disruption in monitoring) then
          if (i<(nrow(AllData)-2) & AllData$Door0_1[i+1]==1){
            #if (i<(nrow(AllData)-40) & AllData$Door0_1[i+1]==1){
            ########################check to see if the UV was activated because of the door
            ########################closing i.e. OutPmA > 5.
            ########################Return a value of 2 if it does, else return a value of 1.
            if (AllData$OutPmA[i+2]>5){
              #if (AllData$OutPmA[i+40]>5){
              DoorSum<-rbind(DoorSum, data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
                                                 YMonth=AllData$YMonth[i],DT=AllData$DT[i],
                                                 OpenDoorTime=AllData$TimeDoorSwitchActivated[i],
                                                 DoorActivatesUV=2))}
            else{
              DoorSum=rbind(DoorSum, data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
                                                YMonth=AllData$YMonth[i],DT=AllData$DT[i],
                                                OpenDoorTime=AllData$TimeDoorSwitchActivated[i],
                                                DoorActivatesUV=1))}
          }
          }
        }
        }
  }}
  })[[3]]
#write.csv(DoorSum,"INSERTROOMNAMEHERE_DoorSum_INSERTDATERUNHEREorTIMINGHERE.csv")
#write.csv(UVSum,"INSERTROOMNAMEHERE_UVSum_INSERTDATERUNHEREorTIMINGHERE.csv")
#####################################################################################
########
#####################################################################################
########
##### End of Stage 1
########################################################################
#####################################################################################
########
#####################################################################################
########
#####################################################################################
########
#####################################################################################
########
#Data Import and Manipulation - Stage 2: Read in Summarized CSV files from UV station
#####################################################################################
########
#####################################################################################
########
DoorSumR1<-read.csv("7-116-StorageRoomDoorSum.csv")
DoorSumR1$Room<-"7-116-StorageRoom"
DoorSumR1$TimeBetween<-0.0000
DoorSumR1$Timing<-'20s'
for (i in 1:(nrow(DoorSumR1)-1)){DoorSumR1$TimeBetween[i+1]<-difftime(DoorSumR1$DT[i+1],
                                                                      DoorSumR1$DT[i], units = c("hours"))}
DoorSumR2<-read.csv("5-104-BathroomDoorSum.csv")
DoorSumR2$Room<-"5-104-Bathroom"
DoorSumR2$TimeBetween<-0.0000
DoorSumR2$Timing<-'20s'
for (i in 1:(nrow(DoorSumR2)-1)){DoorSumR2$TimeBetween[i+1]<-difftime(DoorSumR2$DT[i+1],
                                                                      DoorSumR2$DT[i], units = c("hours"))}
DoorSumR3<-read.csv("5-110-BathroomDoorSum.csv")
DoorSumR3$Room<-"5-110-Bathroom"
DoorSumR3$TimeBetween<-0.0000
DoorSumR3$Timing<-'20s'
for (i in 1:(nrow(DoorSumR3)-1)){DoorSumR3$TimeBetween[i+1]<-difftime(DoorSumR3$DT[i+1],
                                                                      DoorSumR3$DT[i], units = c("hours"))}
DoorSumR4<-read.csv("5-124-UtilityRoomDoorSum.csv")
DoorSumR4$Room<-"5-124-UtilityRoom"
DoorSumR4$TimeBetween<-0.0000
DoorSumR4$Timing<-'20s'
for (i in 1:(nrow(DoorSumR4)-1)){DoorSumR4$TimeBetween[i+1]<-difftime(DoorSumR4$DT[i+1],
                                                                      DoorSumR4$DT[i], units = c("hours"))}
DoorSum<-rbind(DoorSumR1,DoorSumR2,DoorSumR3,DoorSumR4)
DoorSum$Date <- as.Date(DoorSum$Date)
DoorSum$Weekday<-wday(DoorSum$Date, label=TRUE)
DoorSum$Month<-months(DoorSum$Date)
DoorSum$Week<-week(DoorSum$Date)
DoorSum$OpenDoorTimeM<-DoorSum$OpenDoorTime/60
DoorSum$OpenDoorTimeH<-DoorSum$OpenDoorTime/3600
DoorSum$Month<-as.factor(DoorSum$Month)
DoorSum$Month<- factor(DoorSum$Month,levels(DoorSum$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
DoorSum$Date<-as.factor(as.character(DoorSum$Date))
DoorSum$Year<-as.factor(as.character(DoorSum$Year))
DoorSum$Week<-as.factor(as.character(DoorSum$Week))
DoorSum<-DoorSum[complete.cases(DoorSum),]
DoorSum$Day<-yday(DoorSum$Date)
tail(DoorSum)
#######################################################################
DS<-DoorSum %>% group_by(Room, Year,DoorActivatesUV)%>%summarize(n())
colnames(DS)<-c("Room", "Year","DoorActivatesUV","n")
DS<-as.data.frame(DS)
DS$DoorActivatesUV<-as.factor(DS$DoorActivatesUV)
levels(DS$DoorActivatesUV) <- list(Yes="2", No="1")
A_ForPercentOn<-dcast(DS, Room + Year ~ DoorActivatesUV)
A_ForPercentOn$PerCT<-A_ForPercentOn$Yes/(A_ForPercentOn$Yes+A_ForPercentOn$No)*100
#####################################################################################
##########
#### Start of the Plots #############
#####################################################################################
##########
# I used the following commented plot to check to see the difference in the percent door openings
# that led to UV activation when doing both the 1s and 20 sec analysis
# p<- ggplot(ForPercentOn, aes(x=Year, y=PerCT, fill=Timing))+ylab('Percent of Door
# Openings that Activate UV')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
# p<- ggplot(DSII, aes(x=Year, y=n, fill=Timing))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room, scales="free")
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12),
# legend.position="bottom")
# print(p)
#####################################################################################
########
#####################################################################################
########
#Figure 1 - 20 second analysis
################################
p<- ggplot(DS, aes(x=Year, y=n, fill=DoorActivatesUV))+
  ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room, scales="free")
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1),
            axis.text.y = element_text(size=12),
            legend.position="bottom")+
  scale_fill_discrete(name="Door Activates UV")
print(p)
################################
##Figure 2
################################
DS_2<-DoorSum %>% group_by(Room, Year,Month, DoorActivatesUV)%>%summarize(n())
colnames(DS_2)<-c("Room", "Year","Month","DoorActivatesUV", "n")
DS_2<-as.data.frame(DS_2)
DS_2$DoorActivatesUV<-as.factor(DS_2$DoorActivatesUV)
levels(DS_2$DoorActivatesUV) <- list(Yes="2", No="1")
p<- ggplot(DS_2, aes(x=Month, y=n, fill=DoorActivatesUV))+
  ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_grid(Room~Year, scales="free")
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 70, hjust = 1),
            axis.text.y = element_text(size=12),
            legend.position="bottom")+
  scale_fill_discrete(name="Door Activates UV")
print(p)
################################
##Figure 3
################################
p<- ggplot(A_ForPercentOn, aes(x=Year, y=PerCT))+
  ylab('Percent of Door Openings that Activate UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1),
            axis.text.y = element_text(size=12))
print(p)
################################
##Figure 4
################################
B_ForPercentOn<-dcast(DS_2,Room + Year+ Month ~ DoorActivatesUV)
B_ForPercentOn$PerCT<-B_ForPercentOn$Yes/(B_ForPercentOn$Yes+B_ForPercentOn$No)*100
p<- ggplot(B_ForPercentOn, aes(x=Month, y=PerCT))+
  ylab('Percent of Door Openings that Activate UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_grid(Room~Year, scales="free")
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1),
            axis.text.y = element_text(size=12))
print(p)
############Weekly Look -Door Openings- Appendix C 20s##############################
############Weekly Look - UV Activation - Appendix D 20s ##########################
DS_3<-DoorSum %>% group_by(Room, Year, Week, Timing, DoorActivatesUV)%>%summarize(n())
colnames(DS_3)<-c("Room", "Year","Week","Timing", "DoorActivatesUV", "n")
DS_3<-as.data.frame(DS_3)
DS_3$DoorActivatesUV<-as.factor(DS_3$DoorActivatesUV)
levels(DS_3$DoorActivatesUV) <- list(Yes="2", No="1")
DS_3$Week<-as.factor(as.character(DS_3$Week))
DS_3$Week<- factor(DS_3$Week,levels(DS_3$Week)[c(1,12,23,34,45,50,51,52,53,
                                                 2,3,4,5,6,7,8,9,10,11,
                                                 13,14,15,16,17,18,19,20,21,22,
                                                 24,25,26,27,28,29,30,31,32,33,
                                                 35,36,37,38,39,40,41,42,43,44,
                                                 46,47,48,49)])
################################
## Figure C1
################################
p<- ggplot(DS_3[DS_3$Room=='5-104-Bathroom',],
           aes(x=Week, y=n, fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<-p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p + theme(text = element_text(size=14),
             axis.text.x = element_text(size=12, angle = 90, hjust = 1),
             axis.text.y = element_text(size=12))+
  labs(title = "5-104", subtitle = "Bathroom")
print(p)
####################
## Figure D1
####################
C_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-104-Bathroom',],Year+ Week ~ DoorActivatesUV)
C_ForPercentOn$PerCT<-C_ForPercentOn$Yes/(C_ForPercentOn$Yes+C_ForPercentOn$No)*100
p<- ggplot(C_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
                                                      UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "5-104", subtitle = "Bathroom")
print(p)
################################
## Figure C2
################################
p<- ggplot(DS_3[DS_3$Room=='5-110-Bathroom',], aes(x=Week, y=n,
                                                   fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "5-110", subtitle = "Bathroom")
print(p)
################################
## Figure D2
################################
E_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-110-Bathroom' ,],Year+ Week ~ DoorActivatesUV)
E_ForPercentOn$PerCT<-E_ForPercentOn$Yes/(E_ForPercentOn$Yes+E_ForPercentOn$No)*100
p<- ggplot(E_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
                                                      UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "5-110", subtitle = "Bathroom")
print(p)
################################
## Figure C3
################################
p<- ggplot(DS_3[DS_3$Room=='5-124-UtilityRoom',], aes(x=Week, y=n,
                                                      fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "5-124", subtitle = "Utility Room")
print(p)
################################
## Figure D3
################################
G_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-124-UtilityRoom',],Year+ Week ~ DoorActivatesUV)
G_ForPercentOn$PerCT<-G_ForPercentOn$Yes/(G_ForPercentOn$Yes+G_ForPercentOn$No)*100
p<- ggplot(G_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
                                                      UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") + facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "5-124", subtitle = "Utility Room")
print(p)
################################
## Figure C4
################################
p<- ggplot(DS_3[DS_3$Room=='7-116-StorageRoom',], aes(x=Week, y=n, fill=DoorActivatesUV))
+ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "7-116", subtitle = "Storage Room")
print(p)
################################
## Figure D4
################################
H_ForPercentOn<-dcast(DS_3[DS_3$Room=='7-116-StorageRoom',],Year+ Week ~ DoorActivatesUV)
H_ForPercentOn$PerCT<-H_ForPercentOn$Yes/(H_ForPercentOn$Yes+H_ForPercentOn$No)*100
p<- ggplot(H_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
                                                      UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
            axis.text.x = element_text(size=12, angle = 90, hjust = 1),
            axis.text.y = element_text(size=12))+
  labs(title = "7-116", subtitle = "Storage Room")
print(p)
################################
## Figures -> this section was used to examine the per day incidents.
## Unless there is a specific week that we want to look at the overall data is not easy to
## interpret. The code is here if it needs to be run later
################################
################################
#
# DS_4<-DoorSum %>% group_by(Room, Year, Day, DoorActivatesUV)%>%summarize(n())
# colnames(DS_4)<-c("Room", "Year","Day","DoorActivatesUV","n")
# DS_4<-as.data.frame(DS_4)
#
# DS_4$DoorActivatesUV<-as.factor(DS_4$DoorActivatesUV)
# levels(DS_4$DoorActivatesUV) <- list(Yes="2", No="1")
#
# p<- ggplot(DS_4[DS_4$Room=='5-104-Bathroom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='5-110-Bathroom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='5-124-UtilityRoom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# #p<- p + geom_line() +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='7-116-StorageRoom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
#
#
# #################################################################
# # Time Door is Open (TDO) TDOinHperyearfor all rooms
# #################################################################
#
# p<- ggplot(DoorSum, aes(x=Year, y=OpenDoorTimeH, color=Room))+ylab('Time Door is Open
(hours)')+xlab('')
# p<- p + geom_boxplot() #+facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# #################################################################
# # Time Door is Open (TDO) TDOinHpermonth
# #################################################################
#
# p<- ggplot(DoorSum, aes(x=Month, y=OpenDoorTimeH, color=Room))+ylab('Time Door is Open
(hours)')+xlab('')
# p<- p + geom_boxplot() +facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# #################################################################
# # UV Activated by Door Closing (UVActbyDC)
# #################################################################
#
# DS<-DoorSum %>% group_by(Room, Date, Year, Month, Week, DoorActivatesUV)%>%summarize(n())
# colnames(DS)<-c("Room", "Date","Year","Month","Week", "DoorActivatesUV", "n")
# DS$DoorActivatesUV<-as.factor(as.character(DS$DoorActivatesUV))
# levels(DS$DoorActivatesUV) <- list(Yes="2", No="1")
# p<-ggplot(DS, aes(x=DoorActivatesUV, y=n, color=Room))+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# p<-p+geom_boxplot()+facet_wrap(~Year)
# p<-p+ xlab('Does closing the door activate the UV system?')+ylab('Number of instances per day')
# print(p)
#################################################################
# Time between door openings (Figure 5,6 and 7)
#################################################################
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()#+ylim(0,24) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()+ylim(0,24) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()+ylim(0,5) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
# Time between door openings per year per month - not used in the report
#################################################################
#
# p<- ggplot(DoorSum[DoorSum$Year==2017,], aes(x=Month, y=TimeBetween,
color=Room))+ylab('Time Between Door Opening (hours)')+xlab('')
# p<- p + geom_boxplot() +facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#################################################################
UVSumR1<-read.csv("7-116-StorageRoomUVSum.csv")
UVSumR1$Room<-"7-116-StorageRoom"
UVSumR1$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR1)-1)){UVSumR1$TimeBetween[i+1]<-difftime(UVSumR1$DT[i+1],
UVSumR1$DT[i], units = c("hours"))}
UVSumR2<-read.csv("5-104-BathroomUVSum.csv")
UVSumR2$Room<-"5-104-Bathroom"
UVSumR2$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR2)-1)){UVSumR2$TimeBetween[i+1]<-difftime(UVSumR2$DT[i+1],
UVSumR2$DT[i], units = c("hours"))}
UVSumR3<-read.csv("5-110-BathroomUVSum.csv")
UVSumR3$Room<-"5-110-Bathroom"
UVSumR3$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR3)-1)){UVSumR3$TimeBetween[i+1]<-difftime(UVSumR3$DT[i+1],
UVSumR3$DT[i], units = c("hours"))}
UVSumR4<-read.csv("5-124-UtilityRoomUVSum.csv")
UVSumR4$Room<-"5-124-UtilityRoom"
UVSumR4$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR4)-1)){UVSumR4$TimeBetween[i+1]<-difftime(UVSumR4$DT[i+1],
UVSumR4$DT[i], units = c("hours"))}
UVSum<-rbind(UVSumR1,UVSumR2,UVSumR3,UVSumR4)
UVSum$Date <- as.Date(UVSum$Date)
UVSum$Weekday<-wday(UVSum$Date, label=TRUE)
UVSum$Month<-months(UVSum$Date)
UVSum$Week<-week(UVSum$Date)
UVSum$UVAinM<-UVSum$UVAinS/60
UVSum$UVAinH<-UVSum$UVAinS/3600
UVSum$Month<-as.factor(UVSum$Month)
UVSum$Month<- factor(UVSum$Month,levels(UVSum$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
UVSum$Date<-as.factor(as.character(UVSum$Date))
UVSum$Year<-as.factor(as.character(UVSum$Year))
UVSum$Week<-as.factor(as.character(UVSum$Week))
UVSum<-UVSum[complete.cases(UVSum),]
head(UVSum)
tail(UVSum)
#################################################################
# Time UV Activated (TUVA) TUVAinMperyear Figure 8
#################################################################
p<- ggplot(UVSum, aes(x=Year, y=UVAinM, color=Room))+ylab('Time UV is Activated (min)')+xlab('')
p<- p + geom_boxplot() #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")
print(p)
#################################################################
# Time UV Activated (TUVA) TUVAinMpermonth Figure 9
#################################################################
p<- ggplot(UVSum, aes(x=Month, y=UVAinM, color=Room))+ylab('Time UV is Activated (min)')+xlab('')
p<- p + geom_boxplot() +facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")
print(p)
#################################################################
# Time between UV Activation Figure 10
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- ggplot(UVSum, aes(x=Year, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- p + geom_boxplot() #+ylim(0,600)#+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12)
)
print(p)
#################################################################
# Time between UV Activation Figure 11
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- ggplot(UVSum, aes(x=Year, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- p + geom_boxplot() +ylim(0,30)#+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
# Time between UV Activation
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(mins)')+xlab('')
#p<- p + geom_boxplot() +facet_wrap(~Year)+ylim(0,60)
#p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
#print(p)
#################################################################
# Analysis Between September and October 2015
#################################################################
UVSum$Date<-as.Date(UVSum$Date)
DoorSum$Date<-as.Date(DoorSum$Date)
UVSBeg<-UVSum[UVSum$Date>="2015-09-08" & UVSum$Date<="2015-10-02",]
DoorBeg<-DoorSum[DoorSum$Date>="2015-09-08" & DoorSum$Date<="2015-10-08",]
head(UVSBeg)
tail(UVSBeg)
UVSBeg$DT <- strptime(UVSBeg$DT, "%Y-%m-%d %H:%M:%S")
#####################################################################
# Summary to correlate to microbial counts
####################################################################
head(UVSBeg)
UVSBeg$DT <-0
for (i in 1:nrow(UVSBeg)){
if (UVSBeg$UVAinM[i]>4.9){UVSBeg$Cycle[i]="Complete"}
else{UVSBeg$Cycle[i]="Incomplete"}
}
write.csv(UVSBeg, "UVSummaryforSeptOct2015.csv")
write.csv(DoorBeg, "DoorSummaryforSeptOct2015.csv")
UVSforMC<-UVSBeg %>% group_by(Room, Date, Week, Cycle)%>%summarize(n())
colnames(UVSforMC)<-c("Room", "Date","Week", "Cycle", "n")
#################################################################
#Figure 12
#################################################################
p<- ggplot(UVSforMC, aes(x=Date, y=n, linetype=Cycle))+ylab('Number of Times UV is Activated per
Day')+xlab('')
p<- p + geom_line()+geom_point() +facet_wrap(~Room)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
#Figure 13
#################################################################
BC<-read.csv("Bacteria_Counts_AllRooms.csv")
BC$Year<-2015
head(BC)
BC$Month<-substr(BC$Date, 4,6)
BC$Day<-substr(BC$Date, 0,2)
for (i in 1:nrow(BC)){if (BC$Month[i]=='Aug'){BC$MonNum[i]<-8}
else if (BC$Month[i]=='Sep'){BC$MonNum[i]<-9}
else{BC$MonNum[i]<-10}}
BC$Date=c(paste(BC$Day, BC$MonNum, BC$Year, sep='-'))
BC$Date<-as.Date(BC$Date, "%d-%m-%Y")
DoorSum$Room<-as.factor(DoorSum$Room)
BC$Room<- as.factor(BC$Room)
levels(BC$Room)<-c("5-104-Bathroom","5-110-Bathroom","7-116-StorageRoom","5-124-UtilityRoom")
UVSforMC<-UVSBeg %>% group_by(Room, Date)%>%summarize(n(), mean(UVAinS))
colnames(UVSforMC)<-c("Room", "Date","n", "AvUVAinS")
DoorforMC<-DoorBeg %>% group_by(Room, Date)%>%summarize(n(), mean(OpenDoorTimeM))
colnames(DoorforMC)<-c("Room", "Date","nDoor", "AvOpenDoorTimeM")
total <- merge(BC,UVSforMC, by=c("Date","Room"))
total <- merge(total,DoorforMC, by=c("Date","Room"))
total$totalUVinM<-total$n*total$AvUVAinS/60
total$totalDoorinM<-total$nDoor*total$AvOpenDoorTimeM
p<- ggplot(total, aes(x=Date, y=Count, fill=Test_Location))+ylab('Bacterial Counts (barplot)/ \nTime UV is
Activated Per Day in Min (lineplot)')+xlab('')
p<- p + geom_bar(stat = "identity",
position = "stack") + geom_point(aes(x=Date, y=totalUVinM))+geom_line(aes(x=Date,
y=totalUVinM))+facet_wrap(~Room, scales="free_y")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
#################################################################
# Figure 14
#################################################################
p<- ggplot(total, aes(x=Date, y=Count))+ylab('Bacterial Counts (barplot)/ \nTime Door is Open Per Day in
Min (lineplot)')+xlab('')
p<- p + geom_bar(stat = "identity",
position = "stack") + geom_point(aes(x=Date, y=totalDoorinM))+geom_line(aes(x=Date,
y=totalDoorinM))+facet_wrap(~Room, scales="free_y")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
ReportCode_1sec_final.R
library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)
theme_set(theme_bw())
#####################################################################################
############
#Data Import and Manipulation - Stage 1: Collecting and Summarizing CSV files from UV station
#####################################################################################
############
###### 1 SECOND ANALYSIS
########################################################################
system.time({
AllData=data.frame(0,0,0,0,0)
DoorSum<-data.frame(Date=NA, Year=NA, YMonth=NA, OpenDoorTime=NA, DoorActivatesUV=NA)
DoorSum<-DoorSum[complete.cases(DoorSum),]
UVSum<-data.frame(Date=NA,Year=NA, YMonth=NA,UVAinS=NA)
UVSum<-UVSum[complete.cases(UVSum),]
Test<-data.frame(Hour=0,Minute=0,Second=0)
# To loop through directories
dir.names <- list.dirs(full.names = FALSE, recursive = FALSE)
for (room in dir.names){
files <- list.files(file.path(room))
#
# # Looping through files
for (filename in files) {
OldData<-read.csv(file.path(room, filename), check.names=FALSE, header=FALSE)
#The line below was used to test a single file as opposed to the entirety of files provided by
#Class1Inc
#OldData<-read.csv("ST1_180529000001.csv", check.names=FALSE, header=FALSE)
colnames(OldData)<-c("Date","Time", "OutPmA", "OutPA", "Input Door Switch")
OldData$OutPmA<-as.numeric(as.character(OldData$OutPmA))
if (OldData$Date[1]=='Date'){OldData[1,]<-NA}
if (OldData$Date[2]=='Date'){OldData[2,]<-NA}
#OldData<-OldData[complete.cases(OldData), ]
AllData<- OldData[complete.cases(OldData), ]
#AllData<- OldData[seq(1, nrow(OldData), 20), ]
AllData$Room<-room
AllData$Index<-seq.int(nrow(AllData))
AllData$DoorOnOff<-gsub("[[:digit:]]", "", AllData$`Input Door Switch`)
AllData$DoorOnOff<- gsub(' - ','',AllData$DoorOnOff)
AllData$Door0_1<-as.numeric(gsub("[^[:digit:]]", "", AllData$`Input Door Switch`))
AllData<-AllData[complete.cases(AllData), ]
AllData$Date <- as.Date(AllData$Date, "%m/%d/%Y")
AllData$DT <- strptime(paste(AllData$Date,AllData$Time), "%Y-%m-%d %H:%M:%S")
AllData$Year <- substr(AllData$Date, 0, 4)
AllData$YMonth <- substr(AllData$Date, 0, 7)
AllData$DoorActivatesUV<-0
for (i in AllData$Index) {
if (i==1){
AllData$TimeUVActivated=time_length(with(Test, hms(paste(Hour, Minute, Second, sep='
'))),unit="seconds")
}
else {
if (AllData$OutPmA[i]<=5){AllData$TimeUVActivated[i]=time_length(with(Test, hms(paste(Hour,
Minute, Second, sep=' '))), unit="second")}
else {
AllData$TimeUVActivated[i]=AllData$DT[i]-AllData$DT[i-1]+AllData$TimeUVActivated[i-1]
if (i<nrow(AllData)){
if (AllData$OutPmA[i+1]<=5){
UVSum=rbind(UVSum,data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
YMonth=AllData$YMonth[i], DT=AllData$DT[i], UVAinS=AllData$TimeUVActivated[i]))}
}
}
}
##########################Check to see if door is closed (i.e. ON or 1)
if (i==1){
AllData$TimeDoorSwitchActivated=time_length(with(Test, hms(paste(Hour, Minute, Second, sep='
'))), unit="seconds")}
else{
if (AllData$Door0_1[i]==1){
########################If it is closed, then TimeDoorSwitchActivated is set to 0 via the Test
dataframe
AllData$TimeDoorSwitchActivated[i]=time_length(with(Test, hms(paste(Hour, Minute, Second,
sep=' '))), unit="seconds")}
########################However, if the door is open (i.e. OFF or 0) then start the clock to see
########################how long the door is open for
else {AllData$TimeDoorSwitchActivated[i]=AllData$DT[i]-AllData$DT[i-
1]+AllData$TimeDoorSwitchActivated[i-1]
########################Also, if the door is open, but the next time point the door is closed,
and that you
########################can check 40 time points into the future (with the assumption of 1
timepoint=1 sec
########################which is not always the case if there is a disruption in monitoring)
then
#if (i<(nrow(AllData)-2) & AllData$Door0_1[i+1]==1){
if (i<(nrow(AllData)-40) & AllData$Door0_1[i+1]==1){
########################check to see if the UV was activated because of the door closing i.e.
OutPmA > 5.
########################Return a value of 2 if it does, else return a value of 1.
#if (AllData$OutPmA[i+2]>5){
if (AllData$OutPmA[i+40]>5){
DoorSum<-rbind(DoorSum, data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
YMonth=AllData$YMonth[i],DT=AllData$DT[i],OpenDoorTime=AllData$TimeDoorSwitchActivated[i],Doo
rActivatesUV=2))}
else{
DoorSum=rbind(DoorSum, data.frame(Date=AllData$Date[i],Year=AllData$Year[i],
YMonth=AllData$YMonth[i],DT=AllData$DT[i],OpenDoorTime=AllData$TimeDoorSwitchActivated[i],Doo
rActivatesUV=1))}
}
}
}
}
}}
})[[3]]
#write.csv(DoorSum,"INSERTROOMNAMEHERE_DoorSum_INSERTDATERUNHERE.csv")
#write.csv(UVSum,"INSERTROOMNAMEHERE_UVSum_INSERTDATERUNHERE.csv")
#####################################################################################
########
#####################################################################################
########
##### End of Stage 1
########################################################################
#####################################################################################
########
#####################################################################################
########
#####################################################################################
########
#####################################################################################
########
#Data Import and Manipulation - Stage 2: Read in Summarized CSV files from UV station
#####################################################################################
########
#####################################################################################
########
DoorSum1S1<-read.csv("5-104-1secintervalDoor.csv")
DoorSum1S1$Room<-"5-104-Bathroom"
DoorSum1S1$TimeBetween<-0.0000
DoorSum1S1$Timing<-'1s'
for (i in 1:(nrow(DoorSum1S1)-1)){DoorSum1S1$TimeBetween[i+1]<-difftime(DoorSum1S1$DT[i+1],
DoorSum1S1$DT[i], units = c("hours"))}
DoorSum1S2<-read.csv("5-110-1secintervalDoor.csv")
DoorSum1S2$Room<-"5-110-Bathroom"
DoorSum1S2$TimeBetween<-0.0000
DoorSum1S2$Timing<-'1s'
DoorSum1S2<-DoorSum1S2[DoorSum1S2$Year!=1970,]
tail(DoorSum1S2)
DoorSum1S2<-DoorSum1S2[complete.cases(DoorSum1S2),]
#DoorSum1S2$DT<-as.POSIXlt(DoorSum1S2$DT, format ='%Y-%m-%d %I:%M:%S')
for (i in 1:(nrow(DoorSum1S2)-1)){DoorSum1S2$TimeBetween[i+1]<-difftime(DoorSum1S2$DT[i+1],
DoorSum1S2$DT[i], units = c("hours"))}
DoorSum1S3<-read.csv("7-116-1secintervalDoor.csv")
DoorSum1S3$Room<-"7-116-StorageRoom"
DoorSum1S3$TimeBetween<-0.0000
DoorSum1S3$Timing<-'1s'
DoorSum1S3<-DoorSum1S3[DoorSum1S3$Year!=1970,]
tail(DoorSum1S3)
DoorSum1S3<-DoorSum1S3[complete.cases(DoorSum1S3),]
#DoorSum1S2$DT<-as.POSIXlt(DoorSum1S2$DT, format ='%Y-%m-%d %I:%M:%S')
for (i in 1:(nrow(DoorSum1S3)-1)){DoorSum1S3$TimeBetween[i+1]<-difftime(DoorSum1S3$DT[i+1],
DoorSum1S3$DT[i], units = c("hours"))}
DoorSum1S4<-read.csv("5-124-1secintervalDoor.csv")
DoorSum1S4$Room<-"5-124-UtilityRoom"
DoorSum1S4$TimeBetween<-0.0000
DoorSum1S4$Timing<-'1s'
DoorSum1S4<-DoorSum1S4[DoorSum1S4$Year!=1970,]
tail(DoorSum1S4)
DoorSum1S4<-DoorSum1S4[complete.cases(DoorSum1S4),]
#DoorSum1S2$DT<-as.POSIXlt(DoorSum1S2$DT, format ='%Y-%m-%d %I:%M:%S')
for (i in 1:(nrow(DoorSum1S4)-1)){DoorSum1S4$TimeBetween[i+1]<-difftime(DoorSum1S4$DT[i+1],
DoorSum1S4$DT[i], units = c("hours"))}
DoorSum<-rbind(DoorSum1S1,DoorSum1S2,DoorSum1S3, DoorSum1S4)
DoorSum$Date <- as.Date(DoorSum$Date)
DoorSum$Weekday<-wday(DoorSum$Date, label=TRUE)
DoorSum$Month<-months(DoorSum$Date)
DoorSum$Week<-week(DoorSum$Date)
DoorSum$OpenDoorTimeM<-DoorSum$OpenDoorTime/60
DoorSum$OpenDoorTimeH<-DoorSum$OpenDoorTime/3600
DoorSum$Month<-as.factor(DoorSum$Month)
DoorSum$Month<- factor(DoorSum$Month,levels(DoorSum$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
DoorSum$Date<-as.factor(as.character(DoorSum$Date))
DoorSum$Year<-as.factor(as.character(DoorSum$Year))
DoorSum$Week<-as.factor(as.character(DoorSum$Week))
DoorSum<-DoorSum[complete.cases(DoorSum),]
DoorSum$Day<-yday(DoorSum$Date)
tail(DoorSum)
#######################################################################
DS<-DoorSum %>% group_by(Room, Year,DoorActivatesUV, Timing)%>%summarize(n())
colnames(DS)<-c("Room", "Year","DoorActivatesUV","Timing","n")
DS<-as.data.frame(DS)
DS$DoorActivatesUV<-as.factor(DS$DoorActivatesUV)
levels(DS$DoorActivatesUV) <- list(Yes="2", No="1")
A_ForPercentOn<-dcast(DS, Room + Year+ Timing ~ DoorActivatesUV)
A_ForPercentOn$PerCT<-A_ForPercentOn$Yes/(A_ForPercentOn$Yes+A_ForPercentOn$No)*100
#####################################################################################
##########
#####################################################################################
########
#####################################################################################
########
#Figure 1 - 1 second analysis
################################
p<- ggplot(DS, aes(x=Year, y=n, fill=DoorActivatesUV))+
ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room, scales="free")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")+
scale_fill_discrete(name="Door Activates UV")
print(p)
################################
##Figure 2
################################
DS_2<-DoorSum %>% group_by(Room, Year,Month, DoorActivatesUV)%>%summarize(n())
colnames(DS_2)<-c("Room", "Year","Month","DoorActivatesUV", "n")
DS_2<-as.data.frame(DS_2)
DS_2$DoorActivatesUV<-as.factor(DS_2$DoorActivatesUV)
levels(DS_2$DoorActivatesUV) <- list(Yes="2", No="1")
p<- ggplot(DS_2, aes(x=Month, y=n, fill=DoorActivatesUV))+
ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_grid(Room~Year, scales="free")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 70, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")+
scale_fill_discrete(name="Door Activates UV")
print(p)
################################
##Figure 3
################################
p<- ggplot(A_ForPercentOn, aes(x=Year, y=PerCT))+
ylab('Percent of Door Openings that Activate UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Room)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
################################
##Figure 4
################################
B_ForPercentOn<-dcast(DS_2,Room + Year+ Month ~ DoorActivatesUV)
B_ForPercentOn$PerCT<-B_ForPercentOn$Yes/(B_ForPercentOn$Yes+B_ForPercentOn$No)*100
p<- ggplot(B_ForPercentOn, aes(x=Month, y=PerCT))+
ylab('Percent of Door Openings that Activate UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_grid(Room~Year, scales="free")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
############Weekly Look -Door Openings- Appendix C 20s##############################
############Weekly Look - UV Activation - Appendix D 20s ##########################
DS_3<-DoorSum %>% group_by(Room, Year, Week, Timing, DoorActivatesUV)%>%summarize(n())
colnames(DS_3)<-c("Room", "Year","Week","Timing", "DoorActivatesUV", "n")
DS_3<-as.data.frame(DS_3)
DS_3$DoorActivatesUV<-as.factor(DS_3$DoorActivatesUV)
levels(DS_3$DoorActivatesUV) <- list(Yes="2", No="1")
DS_3$Week<-as.factor(as.character(DS_3$Week))
DS_3$Week<- factor(DS_3$Week,levels(DS_3$Week)[c(1,12,23,34,45,50,51,52,53,
2,3,4,5,6,7,8,9,10,11,
13,14,15,16,17,18,19,20,21,22,
24,25,26,27,28,29,30,31,32,33,
35,36,37,38,39,40,41,42,43,44,
46,47,48,49)])
################################
## Figure C1
################################
p<- ggplot(DS_3[DS_3$Room=='5-104-Bathroom',],
aes(x=Week, y=n, fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<-p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p + theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-104", subtitle = "Bathroom")
print(p)
####################
## Figure D1
####################
C_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-104-Bathroom',],Year+ Week ~ DoorActivatesUV)
C_ForPercentOn$PerCT<-C_ForPercentOn$Yes/(C_ForPercentOn$Yes+C_ForPercentOn$No)*100
p<- ggplot(C_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-104", subtitle = "Bathroom")
print(p)
################################
## Figure C2
################################
p<- ggplot(DS_3[DS_3$Room=='5-110-Bathroom',], aes(x=Week, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-110", subtitle = "Bathroom")
print(p)
################################
## Figure D2
################################
E_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-110-Bathroom' ,],Year+ Week ~ DoorActivatesUV)
E_ForPercentOn$PerCT<-E_ForPercentOn$Yes/(E_ForPercentOn$Yes+E_ForPercentOn$No)*100
p<- ggplot(E_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-110", subtitle = "Bathroom")
print(p)
################################
## Figure C3
################################
p<- ggplot(DS_3[DS_3$Room=='5-124-UtilityRoom',], aes(x=Week, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-124", subtitle = "Utility Room")
print(p)
################################
## Figure D3
################################
G_ForPercentOn<-dcast(DS_3[DS_3$Room=='5-124-UtilityRoom',],Year+ Week ~ DoorActivatesUV)
G_ForPercentOn$PerCT<-G_ForPercentOn$Yes/(G_ForPercentOn$Yes+G_ForPercentOn$No)*100
p<- ggplot(G_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") + facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "5-124", subtitle = "Utility Room")
print(p)
################################
## Figure C4
################################
p<- ggplot(DS_3[DS_3$Room=='7-116-StorageRoom',], aes(x=Week, y=n, fill=DoorActivatesUV))+
ylab('Number of Door Openings')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "7-116", subtitle = "Storage Room")
print(p)
################################
## Figure D4
################################
H_ForPercentOn<-dcast(DS_3[DS_3$Room=='7-116-StorageRoom',],Year+ Week ~ DoorActivatesUV)
H_ForPercentOn$PerCT<-H_ForPercentOn$Yes/(H_ForPercentOn$Yes+H_ForPercentOn$No)*100
p<- ggplot(H_ForPercentOn, aes(x=Week, y=PerCT))+ylab('Percent of Door Openings that Activate
UV')+xlab('')
p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year, ncol=1)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 90, hjust = 1),
axis.text.y = element_text(size=12))+
labs(title = "7-116", subtitle = "Storage Room")
print(p)
################################
## Figures -> this section was used to examine the per day incidents.
## Unless there is a specific week that we want to look at the overall data is not easy to
## interpret. The code is here if it needs to be run later
################################
################################
#
# DS_4<-DoorSum %>% group_by(Room, Year, Day, DoorActivatesUV)%>%summarize(n())
# colnames(DS_4)<-c("Room", "Year","Day","DoorActivatesUV","n")
# DS_4<-as.data.frame(DS_4)
#
# DS_4$DoorActivatesUV<-as.factor(DS_4$DoorActivatesUV)
# levels(DS_4$DoorActivatesUV) <- list(Yes="2", No="1")
#
# p<- ggplot(DS_4[DS_4$Room=='5-104-Bathroom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='5-110-Bathroom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='5-124-UtilityRoom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# #p<- p + geom_line() +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# p<- ggplot(DS_4[DS_4$Room=='7-116-StorageRoom',], aes(x=Day, y=n,
fill=DoorActivatesUV))+ylab('Number of Door Openings')+xlab('')
# p<- p + geom_bar(stat="identity", position="dodge") +facet_wrap(~Year,ncol=1)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 90, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
#
#
# #################################################################
# # Time Door is Open (TDO) TDOinHperyearfor all rooms
# #################################################################
#
# p<- ggplot(DoorSum, aes(x=Year, y=OpenDoorTimeH, color=Room))+ylab('Time Door is Open
(hours)')+xlab('')
# p<- p + geom_boxplot() #+facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# #################################################################
# # Time Door is Open (TDO) TDOinHpermonth
# #################################################################
#
# p<- ggplot(DoorSum, aes(x=Month, y=OpenDoorTimeH, color=Room))+ylab('Time Door is Open
(hours)')+xlab('')
# p<- p + geom_boxplot() +facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#
# #################################################################
# # UV Activated by Door Closing (UVActbyDC)
# #################################################################
#
# DS<-DoorSum %>% group_by(Room, Date, Year, Month, Week, DoorActivatesUV)%>%summarize(n())
# colnames(DS)<-c("Room", "Date","Year","Month","Week", "DoorActivatesUV", "n")
# DS$DoorActivatesUV<-as.factor(as.character(DS$DoorActivatesUV))
# levels(DS$DoorActivatesUV) <- list(Yes="2", No="1")
# p<-ggplot(DS, aes(x=DoorActivatesUV, y=n, color=Room))+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# p<-p+geom_boxplot()+facet_wrap(~Year)
# p<-p+ xlab('Does closing the door activate the UV system?')+ylab('Number of instances per day')
# print(p)
#################################################################
# Time between door openings (Figure 5,6 and 7)
#################################################################
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()#+ylim(0,24) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()+ylim(0,24) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
p<- ggplot(DoorSum, aes(x=Year, y=TimeBetween, color=Room))+
ylab('Time Between Door Opening (hours)')+xlab('')
p<- p + geom_boxplot()+ylim(0,5) #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
# Time between door openings per year per month - not used in the report
#################################################################
#
# p<- ggplot(DoorSum[DoorSum$Year==2017,], aes(x=Month, y=TimeBetween,
color=Room))+ylab('Time Between Door Opening (hours)')+xlab('')
# p<- p + geom_boxplot() +facet_wrap(~Year)
# p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
# print(p)
#################################################################
UVSumR1<-read.csv("7-116-StorageRoomUVSum.csv")
UVSumR1$Room<-"7-116-StorageRoom"
UVSumR1$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR1)-1)){UVSumR1$TimeBetween[i+1]<-difftime(UVSumR1$DT[i+1],
UVSumR1$DT[i], units = c("hours"))}
UVSumR2<-read.csv("5-104-BathroomUVSum.csv")
UVSumR2$Room<-"5-104-Bathroom"
UVSumR2$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR2)-1)){UVSumR2$TimeBetween[i+1]<-difftime(UVSumR2$DT[i+1],
UVSumR2$DT[i], units = c("hours"))}
UVSumR3<-read.csv("5-110-BathroomUVSum.csv")
UVSumR3$Room<-"5-110-Bathroom"
UVSumR3$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR3)-1)){UVSumR3$TimeBetween[i+1]<-difftime(UVSumR3$DT[i+1],
UVSumR3$DT[i], units = c("hours"))}
UVSumR4<-read.csv("5-124-UtilityRoomUVSum.csv")
UVSumR4$Room<-"5-124-UtilityRoom"
UVSumR4$TimeBetween<-0.0000
for (i in 1:(nrow(UVSumR4)-1)){UVSumR4$TimeBetween[i+1]<-difftime(UVSumR4$DT[i+1],
UVSumR4$DT[i], units = c("hours"))}
UVSum<-rbind(UVSumR1,UVSumR2,UVSumR3,UVSumR4)
UVSum$Date <- as.Date(UVSum$Date)
UVSum$Weekday<-wday(UVSum$Date, label=TRUE)
UVSum$Month<-months(UVSum$Date)
UVSum$Week<-week(UVSum$Date)
UVSum$UVAinM<-UVSum$UVAinS/60
UVSum$UVAinH<-UVSum$UVAinS/3600
UVSum$Month<-as.factor(UVSum$Month)
UVSum$Month<- factor(UVSum$Month,levels(UVSum$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
UVSum$Date<-as.factor(as.character(UVSum$Date))
UVSum$Year<-as.factor(as.character(UVSum$Year))
UVSum$Week<-as.factor(as.character(UVSum$Week))
UVSum<-UVSum[complete.cases(UVSum),]
head(UVSum)
tail(UVSum)
#################################################################
# Time UV Activated (TUVA) TUVAinMperyear Figure 8
#################################################################
p<- ggplot(UVSum, aes(x=Year, y=UVAinM, color=Room))+ylab('Time UV is Activated (min)')+xlab('')
p<- p + geom_boxplot() #+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")
print(p)
#################################################################
# Time UV Activated (TUVA) TUVAinMpermonth Figure 9
#################################################################
p<- ggplot(UVSum, aes(x=Month, y=UVAinM, color=Room))+ylab('Time UV is Activated (min)')+xlab('')
p<- p + geom_boxplot() +facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12),
legend.position="bottom")
print(p)
#################################################################
# Time between UV Activation Figure 10
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- ggplot(UVSum, aes(x=Year, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- p + geom_boxplot() #+ylim(0,600)#+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12)
)
print(p)
#################################################################
# Time between UV Activation Figure 11
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- ggplot(UVSum, aes(x=Year, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(hours)')+xlab('')
p<- p + geom_boxplot() +ylim(0,30)#+facet_wrap(~Year)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
# Time between UV Activation
#################################################################
#p<- ggplot(UVSum, aes(x=Month, y=TimeBetween, color=Room))+ylab('Time Between UV Activation
(mins)')+xlab('')
#p<- p + geom_boxplot() +facet_wrap(~Year)+ylim(0,60)
#p<-p+ theme(text = element_text(size=14),
# axis.text.x = element_text(size=12, angle = 45, hjust = 1),
# axis.text.y = element_text(size=12))
#print(p)
#################################################################
# Analysis Between September and October 2015
#################################################################
UVSum$Date<-as.Date(UVSum$Date)
DoorSum$Date<-as.Date(DoorSum$Date)
UVSBeg<-UVSum[UVSum$Date>="2015-09-08" & UVSum$Date<="2015-10-02",]
DoorBeg<-DoorSum[DoorSum$Date>="2015-09-08" & DoorSum$Date<="2015-10-08",]
head(UVSBeg)
tail(UVSBeg)
UVSBeg$DT <- strptime(UVSBeg$DT, "%Y-%m-%d %H:%M:%S")
#####################################################################
# Summary to correlate to microbial counts
####################################################################
head(UVSBeg)
UVSBeg$DT <-0
for (i in 1:nrow(UVSBeg)){
if (UVSBeg$UVAinM[i]>4.9){UVSBeg$Cycle[i]="Complete"}
else{UVSBeg$Cycle[i]="Incomplete"}
}
write.csv(UVSBeg, "UVSummaryforSeptOct2015.csv")
write.csv(DoorBeg, "DoorSummaryforSeptOct2015.csv")
UVSforMC<-UVSBeg %>% group_by(Room, Date, Week, Cycle)%>%summarize(n())
colnames(UVSforMC)<-c("Room", "Date","Week", "Cycle", "n")
#################################################################
#Figure 12
#################################################################
p<- ggplot(UVSforMC, aes(x=Date, y=n, linetype=Cycle))+ylab('Number of Times UV is Activated per
Day')+xlab('')
p<- p + geom_line()+geom_point() +facet_wrap(~Room)
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
#Figure 13
#################################################################
BC<-read.csv("Bacteria_Counts_AllRooms.csv")
BC$Year<-2015
head(BC)
BC$Month<-substr(BC$Date, 4,6)
BC$Day<-substr(BC$Date, 0,2)
for (i in 1:nrow(BC)){if (BC$Month[i]=='Aug'){BC$MonNum[i]<-8}
else if (BC$Month[i]=='Sep'){BC$MonNum[i]<-9}
else{BC$MonNum[i]<-10}}
BC$Date=c(paste(BC$Day, BC$MonNum, BC$Year, sep='-'))
BC$Date<-as.Date(BC$Date, "%d-%m-%Y")
DoorSum$Room<-as.factor(DoorSum$Room)
BC$Room<- as.factor(BC$Room)
levels(BC$Room)<-c("5-104-Bathroom","5-110-Bathroom","7-116-StorageRoom","5-124-UtilityRoom")
UVSforMC<-UVSBeg %>% group_by(Room, Date)%>%summarize(n(), mean(UVAinS))
colnames(UVSforMC)<-c("Room", "Date","n", "AvUVAinS")
DoorforMC<-DoorBeg %>% group_by(Room, Date)%>%summarize(n(), mean(OpenDoorTimeM))
colnames(DoorforMC)<-c("Room", "Date","nDoor", "AvOpenDoorTimeM")
total <- merge(BC,UVSforMC, by=c("Date","Room"))
total <- merge(total,DoorforMC, by=c("Date","Room"))
total$totalUVinM<-total$n*total$AvUVAinS/60
total$totalDoorinM<-total$nDoor*total$AvOpenDoorTimeM
p<- ggplot(total, aes(x=Date, y=Count, fill=Test_Location))+ylab('Bacterial Counts (barplot)/ \nTime UV is
Activated Per Day in Min (lineplot)')+xlab('')
p<- p + geom_bar(stat = "identity",
position = "stack") + geom_point(aes(x=Date, y=totalUVinM))+geom_line(aes(x=Date,
y=totalUVinM))+facet_wrap(~Room, scales="free_y")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)
#################################################################
#################################################################
# Figure 14
#################################################################
p<- ggplot(total, aes(x=Date, y=Count))+ylab('Bacterial Counts (barplot)/ \nTime Door is Open Per Day in
Min (lineplot)')+xlab('')
p<- p + geom_bar(stat = "identity",
position = "stack") + geom_point(aes(x=Date, y=totalDoorinM))+geom_line(aes(x=Date,
y=totalDoorinM))+facet_wrap(~Room, scales="free_y")
p<-p+ theme(text = element_text(size=14),
axis.text.x = element_text(size=12, angle = 45, hjust = 1),
axis.text.y = element_text(size=12))
print(p)