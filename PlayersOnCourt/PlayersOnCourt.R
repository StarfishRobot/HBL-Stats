
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(png)
library(googlesheets4)

#get PBP data
PBP<-data.frame()
for(i in c(2025:2025)){
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

PBP<-PBP%>%filter(Event%in%c("Goal","Miss", "KeeperChange"))
Game<-PBP$gameID[1]
HomeGoalie<-"Empty Net"
AwayGoalie<-"Empty Net"

PBP$HomeGoale<-ifelse(PBP$teamName==PBP$homeTeam,
                      ifelse(PBP$Event=="KeeperChange",
                        ifelse(is.na(PBP$playerName), "Empty Net", "Goalie In"), NA), NA)
PBP$AwayGoalie<-ifelse(PBP$teamName==PBP$awayTeam,
                      ifelse(PBP$Event=="KeeperChange",
                             ifelse(is.na(PBP$playerName), "Empty Net", "Goalie In"), NA), NA)

PBP<-PBP%>%fill(HomeGoale)%>%fill(AwayGoalie)

PBP%>%select(season,
             teamName,
             homeTeam, 
             awayTeam,
             HomeFieldPlayers,
             AwayFieldPlayers,
             Event, 
             HomeGoale, 
             AwayGoalie)%>%
  filter(Event%in%c("Goal", "Miss"))%>%
  mutate(Players=ifelse(teamName==homeTeam, 
                        paste0(HomeFieldPlayers, "v", AwayFieldPlayers),
                        paste0(AwayFieldPlayers, "v", HomeFieldPlayers)))%>%
  mutate(GoalStatus=ifelse(teamName==homeTeam, HomeGoale, AwayGoalie))%>%
  mutate(Score=ifelse(Event=="Goal", 1, 0))%>%
  select(season, teamName, Score, Players, GoalStatus)%>%
  group_by(Players, GoalStatus)%>%
  summarise(Rate=sum(Score)/n(), Count=n())%>%
  filter(Count>10)



