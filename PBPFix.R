library(dplyr)
library(tidyr)
PBP<-data.frame()
for(i in c(2017:2025)){
  PBP<-PBP%>%bind_rows(read.csv(paste0("https://raw.githubusercontent.com/StarfishRobot/HBL-Stats/refs/heads/main/PlayByPlay/HBL-Box-PBP-DE-",i,".csv")))
}


CurGame<-PBP$gameID[1]
CurrentSusp<-data.frame(EndSecond=integer(), Team=character())
HomePlayers<-6
AwayPlayers<-6
for(i in c(1:nrow(PBP))){
  cat("\r", paste0(replicate(i/nrow(PBP)*10, "="), ">", collapse=""))
  if(PBP$gameID[i]!=CurGame){
    HomePlayers<-6
    AwayPlayers<-6
    CurrentSusp<-data.frame(EndSecond=integer(), Team=character())
    CurGame<-PBP$gameID[i]
  }
  if(PBP$eventType[i]=="suspension"){
    SuspLength<-ifelse(PBP$eventSubType[i]=="twoMinutes", 2, 4)
    if(PBP$teamName[i]==PBP$homeTeam[i]){
      CurrentSusp<-CurrentSusp%>%bind_rows(data.frame(EndSecond=(PBP$eventMinute[i]*60)+PBP$eventSecond[i]+SuspLength*60, Team="Home"))
    }else{
      CurrentSusp<-CurrentSusp%>%bind_rows(data.frame(EndSecond=(PBP$eventMinute[i]*60)+PBP$eventSecond[i]+SuspLength*60, Team="Away"))
    }
  }
  CurrentSusp<-CurrentSusp%>%filter(EndSecond>(PBP$eventMinute[i]*60+PBP$eventSecond[i]))
  HomePlayers<-6-nrow(CurrentSusp%>%filter(Team=="Home"))
  AwayPlayers<-6-nrow(CurrentSusp%>%filter(Team=="Away"))
  PBP$HomeFieldPlayers[i]<-HomePlayers+PBP$HomeEmpty[i]
  PBP$AwayFieldPlayers[i]<-AwayPlayers+PBP$AwayEmpty[i]
}

for(i in c(2017:2025)){
  write.csv(PBP%>%filter(season==i), paste0("D:/OneDrive/Public/HBL-Stats/PlayByPlay/HBL-Box-PBP-DE-",i,".csv"), row.names = F)
}
