library(dplyr)
library(plotly)
library(hms)
PBP<-data.frame()
for(i in c(2017:2025)){
  PBP<-PBP%>%bind_rows(read.csv(paste0("D:/OneDrive/Public/HBL-Stats/PlayByPlay/HBL-Box-PBP-DE-",i,"-2.csv")))
}



PBP<-PBP%>%mutate(eventMinute = as.integer(stringr::str_split(string = gameTime, pattern = ":", simplify = T)[,1]))
PBP<-PBP%>%mutate(eventSecond = as.integer(stringr::str_split(string = gameTime, pattern = ":", simplify = T)[,2]))


for(i in c(1:nrow(PBP))){
  cat("\r", paste0(replicate(i/nrow(PBP)*10, "="), ">", collapse=""))
  PBP$Possesion[i]<-nrow(PBP%>%
                           filter(teamName==PBP$teamName[i] & 
                                    gameID==PBP$gameID[i] & 
                                    eventOrder<=PBP$eventOrder[i] &
                                    (eventType=="goal" | 
                                       eventType=="technicalFault" | 
                                       eventType=="turnover")))
}



CurGame<-PBP$gameID[1]
HomeEmpty<-0
AwayEmpty<-0
for(i in c(1:nrow(PBP))){
  cat("\r", paste0(replicate(i/nrow(PBP)*10, "="), ">", collapse=""))
  if(PBP$gameID[i]!=CurGame){
    HomeEmpty<-0
    AwayEmpty<-0
    CurGame<-PBP$gameID[i]
  }
  if(PBP$eventText[i]=="Torwartwechsel"){
    if(is.na(PBP$playerName[i])){
      if(PBP$teamName[i]==PBP$homeTeam[i]){
        HomeEmpty<-1
      }else{
        AwayEmpty<-1
      }
    }else{
      if(PBP$teamName[i]==PBP$homeTeam[i]){
        HomeEmpty<-0
      }else{
        AwayEmpty<-0
      }
    }
  }
  PBP$HomeEmpty[i]<-HomeEmpty
  PBP$AwayEmpty[i]<-AwayEmpty
}


#Get Player count
CurGame<-PBP$gameID[1]
HomePlayers<-6
AwayPlayers<-6
CurrentSusp<-data.frame()

for(i in c(1:nrow(PBP))){
  cat("\r", paste0(replicate(i/nrow(PBP)*10, "="), ">", collapse=""))
  if(PBP$gameID[i]!=CurGame){
    HomePlayers<-6
    AwayPlayers<-6
    CurrentSusp<-data.frame()
    CurGame<-PBP$gameID[i]
  }
  if(PBP$eventType[i]=="suspension"){
    SuspLength<-ifelse(PBP$eventSubType[i]=="twoMinutes", 2, 4)
      if(PBP$teamName[i]==PBP$homeTeam[i]){
        CurrentSusp<-CurrentSusp%>%bind_rows(data.frame(Time=parse_hms(c(paste0("00:", PBP$eventMinute[i]+SuspLength, ":", PBP$eventSecond[i]))), Team="Home"))
        HomePlayers<-HomePlayers-1
      }else{
        CurrentSusp<-CurrentSusp%>%bind_rows(data.frame(Time=parse_hms(c(paste0("00:", PBP$eventMinute[i]+SuspLength, ":", PBP$eventSecond[i]))), Team="Away"))
        AwayPlayers<-AwayPlayers-1
      }
    }else{
      if(nrow(CurrentSusp)>0){
        EndedSusp<-CurrentSusp%>%filter(parse_hms(c(paste0("00:", PBP$eventMinute[i], ":", PBP$eventSecond[i])))>=Time)
        HomePlayers<-HomePlayers+nrow(EndedSusp%>%filter(Team=="Home"))
        AwayPlayers<-AwayPlayers+nrow(EndedSusp%>%filter(Team=="Away"))
        CurrentSusp<-CurrentSusp%>%filter(parse_hms(c(paste0("00:", PBP$eventMinute[i], ":", PBP$eventSecond[i])))<Time)
      }
    }
  
  PBP$HomeFieldPlayers[i]<-HomePlayers+PBP$HomeEmpty[i]
  PBP$AwayFieldPlayers[i]<-AwayPlayers+PBP$AwayEmpty[i]
}
 





for(i in c(2017:2025)){
  write.csv(PBP%>%filter(season==i), paste0("D:/OneDrive/Public/HBL-Stats/PlayByPlay/HBL-Box-PBP-DE-", i, ".csv"), row.names = F)
}



EmptyNetMiss<-PBP%>%filter((grepl("Fehlwurf", eventText) | eventType=="turnover" |eventType=="technicalFault") & ((teamName==homeTeam & HomeEmpty==1)|(teamName==awayTeam & AwayEmpty==1)))%>%select(season)%>%table()

EmptyNetMake<-PBP%>%filter((grepl("Tor ", eventText)) & ((teamName==homeTeam & HomeEmpty==1)|(teamName==awayTeam & AwayEmpty==1)))%>%select(season)%>%table()

EmptyNetGiven<-PBP%>%filter((grepl("Tor ", eventText)) & ((teamName!=homeTeam & HomeEmpty==1)|(teamName!=awayTeam & AwayEmpty==1)))%>%select(season)%>%table()

plot_ly(x=c(2017:2025), y=EmptyNetMake, type="bar", name="Empty Net Goals", text=EmptyNetMake)%>%
  add_trace(x=c(2017:2025), y=EmptyNetMiss, type="bar",name="Empty Net Misses", text=EmptyNetMiss)%>%
  add_trace(x=c(2017:2025), y=EmptyNetGiven, type="bar",name="Empty Net Conceded", text=EmptyNetGiven)


SevenMiss<-PBP%>%filter((grepl("Fehlwurf", eventText) | eventType=="turnover" |eventType=="technicalFault") & ((teamName==homeTeam & HomeFieldPlayers==7)|(teamName==awayTeam & AwayFieldPlayers==7)))%>%select(season)%>%table()

SevenMake<-PBP%>%filter((grepl("Tor ", eventText)) & ((teamName==homeTeam & HomeEmpty==1 & HomeFieldPlayers==7)|(teamName==awayTeam & AwayEmpty==1 & AwayFieldPlayers==7)))%>%select(season)%>%table()
 
SevenGiven<-PBP%>%filter((grepl("Tor ", eventText)) & ((teamName!=homeTeam & HomeFieldPlayers==7)|(teamName!=awayTeam & AwayFieldPlayers==7)))%>%select(season)%>%table()

plot_ly(x=c(2017:2025), y=SevenMake, type="bar", name="7v6 Goals", text=SevenMake)%>%
  add_trace(x=c(2017:2025), y=SevenMiss, type="bar",name="7v6 Misses", text=SevenMiss)%>%
  add_trace(x=c(2017:2025), y=SevenGiven, type="bar",name="7v6 Counter Goals", text=SevenGiven)


ManDownGoals<-PBP%>%filter((grepl("Tor ", eventText))& ((teamName==homeTeam & HomeFieldPlayers<AwayFieldPlayers)|(teamName==awayTeam & AwayFieldPlayers<HomeFieldPlayers)))%>%
  select(playerName)%>%table()%>%as.data.frame()
ManDownMiss<-PBP%>%filter((grepl("Fehlwurf ", eventText))& ((teamName==homeTeam & HomeFieldPlayers<AwayFieldPlayers)|(teamName==awayTeam & AwayFieldPlayers<HomeFieldPlayers)))%>%
  select(playerName)%>%table()%>%as.data.frame()
ManDownGoals<-ManDownGoals%>%left_join(ManDownMiss, by=c("playerName"="playerName"))
ManDownGoals$Rate<-ManDownGoals$Freq.x/(ManDownGoals$Freq.x+ManDownGoals$Freq.y)
ManDownGoals%>%arrange(desc(Freq.x))%>%head()
