
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(png)
library(googlesheets4)

#get PBP data
PBP<-data.frame()
for(i in c(2017:2026)){
  PBP<-PBP%>%bind_rows(read.csv(paste0("https://raw.githubusercontent.com/StarfishRobot/HBL-Stats/refs/heads/main/PlayByPlay/HBL-Box-PBP-DE-",i,".csv")))
}
PlayerRoster<-PBP%>%select(playerId, playerName)%>%
  filter(playerName!="")%>% 
  group_by(playerId)%>%
  filter(row_number() == 1)
#set event types
EventMatch<-c('4-Minuten Strafe'='Penalty', 
              '7-Meter Strafwurf erfolgreich'='Goal', 
              '7-Meter Strafwurf herausgeholt'='7-Meter Earned', 
              '7-Meter Strafwurf verfehlt'='Miss', 
              '7-Meter Strafwurf verursacht'='7-Meter Given',
              'Assist'='Assist', 'Auszeit eines Teams'='Timeout', 
              'Ballverlust'='Turnover', 'Blaue Karte'='Card', 
              'Block'='Block', 
              'Fehlwurf'='Miss', 
              'Fehlwurf aus dem Rückraum'='Miss', 
              'Fehlwurf aus der eigenen Hälfte'='Miss', 
              'Fehlwurf aus der Nahwurfzone'='Miss',
              'Fehlwurf von außen'='Miss',
              'Gelbe Karte'='Card', 
              'Passives Spiel'='Turnover',
              'Rote Karte'='Card', 
              'Technischer Fehler'='Turnover', 
              'Technischer Fehler (Ball)'='Turnover', 
              'Technischer Regelfehler'='Turnover', 
              'Tor'='Goal', 
              'Tor aus dem Rückraum'='Goal', 
              'Tor aus der eigenen Hälfte'='Goal', 
              'Tor aus der Nahwurfzone'='Goal', 
              'Tor von außen'='Goal', 
              'Torwartwechsel'='KeeperChange', 
              'Zwei-Minuten Strafe'='Penalty')


PBP<-PBP%>%mutate(Event=recode(eventText, !!!EventMatch))

#Determine man up/shorthand situations
PBP$Modifier<-ifelse(PBP$teamName==PBP$homeTeam, 
                     ifelse(PBP$HomeFieldPlayers>PBP$AwayFieldPlayers, "Man Up", 
                            ifelse(PBP$HomeFieldPlayers<PBP$AwayFieldPlayers, "Shorthanded", "Even")),
                     ifelse(PBP$HomeFieldPlayers>PBP$AwayFieldPlayers, "Shorthanded", 
                            ifelse(PBP$HomeFieldPlayers<PBP$AwayFieldPlayers, "Man Up", "Even")))


#filter only for shooting events
Events<-PBP%>%filter(Event=="Goal" | Event=="Miss")

Events<-Events%>%filter(eventText!="Tor" & eventText!="Fehlwurf")

Rates<-Events%>%
  select(Event, eventSubType, Modifier)%>%
  group_by(eventSubType, Modifier, Event)%>%
  summarise(n=n())%>%
  pivot_wider(names_from = Event, values_from = n)%>%
  mutate(Rate=Goal/(Goal+Miss))%>%select(eventSubType, Modifier, Rate)


Events<-Events%>%left_join(Rates, by=c("eventSubType"="eventSubType", "Modifier"="Modifier"))
Events$Score<-ifelse(Events$Event=="Goal", 1, 0)

Basic.Exp<-Events%>%#filter(season==2025)%>%
  select(season, playerId, Rate, Score)%>%
  group_by(season, playerId)%>%
  summarise(Shots = n(),Goals=sum(Score), ExpGoals=sum(Rate))%>%
  mutate(Dif=Goals/ExpGoals, GoalRate=Goals/Shots)%>%
  mutate(ExGlperShot=ExpGoals/Shots)%>%
  filter(Shots>10)%>%
  arrange(desc(ExGlperShot))#%>%
  #left_join(PBP%>%filter(playerName!="")%>%select(playerName, playerId)%>%distinct())


Events$y<-ifelse(Events$x<50, 100-Events$y, Events$y)
Events$x<-ifelse(Events$x<50, 100-Events$x, Events$x)
Events$x<-ifelse(grepl("eigenen Hälfte", Events$eventText),
                 100-Events$x, Events$x)

Location<-Events%>%filter(!is.na(x))%>%filter(season>=2024)
Location$x<-40*(Location$x/100)
Location$y<-20*(Location$y/100)
fig<-plot_ly()
fig<-fig%>%add_trace(x=Location$x, y=Location$y,type = 'scatter',mode = 'markers', text=Location$eventText)
fig
ShotZones<-data.frame()
for(t in Location%>%pull(Modifier)%>%unique()){
  for(i in c(0:40)){
    for(w in c(0:20)){
      Zone<-Location%>%filter(Modifier==t & x>=i & x<i+1 & y>=w & y<w+1)
      ZoneRate<-ifelse(Zone%>%nrow()<=10, 0, Zone%>%filter(Event=="Goal")%>%nrow()/Zone%>%nrow())
      
      ShotZones<-ShotZones%>%bind_rows(data.frame(x=i, y=w, Modifier=t, ZoneRate=ZoneRate, Shots=Zone%>%nrow()))
    }
  }
}

Exact.Exp<-Events%>%filter(season>=2024)%>%
  select(season, playerId,eventSubType, Rate, Score, x, y, Modifier)%>%
  mutate(x=round(40*x/100, 0))%>%
  mutate(y=round(20*y/100, 0))%>%
  left_join(ShotZones, by=c("x"="x", "y"="y", "Modifier"="Modifier"))%>%
  group_by(season, playerId)%>%
  filter(!is.na(ZoneRate))%>%
  summarise(Shots = n(),Goals=sum(Score), ExpGoals=sum(ZoneRate))%>%
  mutate(Dif=Goals/ExpGoals, GoalRate=Goals/Shots)%>%
  mutate(ExGlperShot=ExpGoals/Shots)%>%
  filter(Shots>10)%>%
  arrange(desc(Dif))#%>%
  #left_join(PBP%>%filter(playerName!="")%>%select(playerName, playerId)%>%distinct())

Team.Exp.Goals<-Events%>%select(teamName, playerId, season)%>%distinct()%>%filter(season==2026)%>%
  left_join(Basic.Exp%>%select(playerId, Shots, Goals, ExpGoals, Dif))%>%
  left_join(Exact.Exp%>%select(season, playerId, ExpGoals, Dif)%>%
              rename(Exact.Exp=ExpGoals, 
                     Exact.Dif=Dif),
            by=c("playerId"="playerId", "season"="season"))%>%
  filter(!is.na(Shots) & season>=2024 & !is.na(Exact.Exp))%>%
  mutate(MeanExp=(ExpGoals+Exact.Exp)/2)%>%
  mutate(Avg.Dif=Goals/MeanExp)%>%
  select(season, teamName, Shots, Goals, MeanExp, Avg.Dif)%>%
  #mutate(Exp.Dif=abs(Dif-Exact.Dif))%>%
  group_by(teamName)%>%
  summarise(Shots = sum(Shots),Goals=sum(Goals), ExpGoals=sum(MeanExp))%>%
mutate(Avg.Dif=Goals/ExpGoals)%>%
  arrange(desc(Avg.Dif))
  # select(season, teamName, playerName, Shots, Goals, MeanExp, Avg.Dif)

Player.Exp.Goals<-Events%>%select(teamName, playerId, season)%>%distinct()%>%filter(season>=2024)%>%
  left_join(Basic.Exp%>%select(playerId, Shots, Goals, ExpGoals, Dif))%>%
  left_join(Exact.Exp%>%select(season, playerId, ExpGoals, Dif)%>%
              rename(Exact.Exp=ExpGoals, 
                     Exact.Dif=Dif),
            by=c("playerId"="playerId", "season"="season"))%>%
  filter(!is.na(Shots) & season>=2024 & !is.na(Exact.Exp))%>%
  mutate(MeanExp=(ExpGoals+Exact.Exp)/2)%>%
  mutate(Avg.Dif=Goals/MeanExp)%>%
  #mutate(Exp.Dif=abs(Dif-Exact.Dif))%>%
  left_join(PlayerRoster)%>%
  arrange(desc(teamName))%>%
  select(season, teamName, playerName, Shots, Goals, MeanExp, Avg.Dif)
  


ShotbyShot<-Events%>%mutate(x=round(40*x/100, 0))%>%
  mutate(y=round(20*y/100, 0))%>%
  left_join(ShotZones, by=c("x"="x", "y"="y", "Modifier"="Modifier"))
ShotbyShot%>%filter(gameID=="3a7d1104-79f3-11f1-a4af-a9813bd3aa40")%>%
  mutate(AvgRate=(Rate+ZoneRate)/2)%>%
  select(teamName, playerName, Score, AvgRate)%>%
  group_by(teamName)%>%
  summarise(`Goals/Shots`=paste0(sum(Score), "/", n()), ExpGoals=sum(AvgRate))
