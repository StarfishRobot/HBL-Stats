library(dplyr)
library(tidyr)
library(googlesheets4)

PBP<-data.frame()
for(i in c(2024:2026)){
  PBP<-PBP%>%bind_rows(read.csv(paste0("d:/OneDrive/Public/HBL-Stats/PlayByPlay/HBL-Box-PBP-DE-",i,".csv")))
}

EventMatch<-c('4-Minuten Strafe'='Penalty',
              '7-Meter Strafwurf erfolgreich'='7-Meter Goal', 
              '7-Meter Strafwurf herausgeholt'='7-Meter Earned', 
              '7-Meter Strafwurf verfehlt'='7-Meter Miss', 
              '7-Meter Strafwurf verursacht'='7-Meter Given', 
              'Assist'='Assist', 
              'Auszeit eines Teams'='Timeout', 
              'Ballverlust'='Turnover', 
              'Blaue Karte'='Card',
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


PBP<-PBP%>%mutate(Event=recode(eventText, !!!EventMatch))%>%filter(Event=="Goal" | Event=="Miss")%>%select(season, teamName, playerName, eventText, eventSubType, x, y, Event, playerId)
PlayerList<-PBP%>%select(playerId, playerName)%>%distinct()%>%rename(Name=playerName)%>%group_by(playerId)%>%filter(row_number() == 1)
PBP<-PBP%>%left_join(PlayerList)
PBP$playerName<-PBP$Name
PBP$Name<-NULL


PBP$y<-ifelse(PBP$x<50, 100-PBP$y, PBP$y)
PBP$x<-ifelse(PBP$x<50, 100-PBP$x, PBP$x)
PBP$x<-ifelse(grepl("eigenen Hälfte", PBP$eventText),
                 100-PBP$x, PBP$x)

write_sheet(PBP, ss="1szAxY0xl-rOOrqGGQ0zJC3MLsKm6dPYT-KCSWtG8CFA", sheet="Shots")
