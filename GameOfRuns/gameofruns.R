
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(png)
library(googlesheets4)

#get historical data
ELO<-read_sheet("1DJyTtMb1F89J0dcP3GVmRDuyVe4kbhvxmL8HJET4cC8", sheet = "Sheet2", range="A:L")%>%
  rename("Home.Score"=`Home Score`, 
         "Away.Score"=`Away Score`,
         "Pred.Margin" = `Pred Margin`,
         "Home.Win.."= `Home Win %`,
         "Away.Win.." = `Away Win %`,
         "Home.ELO" = `Home ELO`,
         "Away.ELO" = `Away ELO`)
ELO$WinningTeam<-ifelse(ELO$Home.Score>ELO$Away.Score, ELO$Home, ifelse(ELO$Away.Score>ELO$Home.Score, ELO$Away, "Tie"))

#get PBP data
PBP<-data.frame()
for(i in c(2017:2026)){
  PBP<-PBP%>%bind_rows(read.csv(paste0("https://raw.githubusercontent.com/StarfishRobot/HBL-Stats/refs/heads/main/PlayByPlay/HBL-Box-PBP-DE-",i,".csv")))
}
PBP<-PBP%>%left_join(ELO%>%select(GameId, WinningTeam), by=c("gameID"="GameId"))
PBP$Winner<-ifelse(PBP$WinningTeam==PBP$teamName, "Y", ifelse(PBP$WinningTeam=="Tie", "Tie", "N"))

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
PBP<-PBP%>%filter(Event=="Goal")
GameId<-PBP$gameID[1]
HomeStreak<-0
AwayStreak<-0
PBP$Run<-0
for(i in c(1:nrow(PBP))){
  cat("\r", paste0(replicate(i/nrow(PBP)*10, "="), ">", collapse=""))
  if(GameId!=PBP$gameID[i]){
    GameId<-PBP$gameID[i]
    HomeStreak<-0
    AwayStreak<-0
  }
  if(PBP$teamName[i]==PBP$homeTeam[i]){
    HomeStreak<-HomeStreak+1
    AwayStreak<-0
    PBP$Run[i]<-HomeStreak
  }else{
    HomeStreak<-0
    AwayStreak<-AwayStreak+1
    PBP$Run[i]<-AwayStreak
  }
  
}

All.Runs<-PBP%>%select(Run, Winner)%>%
  filter(!is.na(Winner))%>%
  mutate(Val=1)%>%
  pivot_wider(names_from = Winner, values_from = Val, values_fn = sum)%>%
  replace_na(list(Y=0, N=0, Tie=0))%>%
  mutate(Rate=Y/(Y+N+Tie))
plot_ly()%>%
  add_trace(type="bar", 
            x = All.Runs$Run, 
            y=All.Runs$Y/(All.Runs$Y+All.Runs$N+All.Runs$Tie),  
            text=paste0(round((All.Runs$Y/(All.Runs$Y+All.Runs$N+All.Runs$Tie))*100, 1), "%"), 
            name="Victories",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#badc58"))%>%
  add_trace(type="bar", 
            x = All.Runs$Run, 
            y=All.Runs$Tie/(All.Runs$Y+All.Runs$N+All.Runs$Tie), 
            text=paste0(round((All.Runs$Tie/(All.Runs$Y+All.Runs$N+All.Runs$Tie))*100, 1), "%"), 
            name="Ties",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#d0d0d0"))%>%
  add_trace(type="bar", 
            x = All.Runs$Run, 
            y=All.Runs$N/(All.Runs$Y+All.Runs$N+All.Runs$Tie), 
            text=paste0(round((All.Runs$N/(All.Runs$Y+All.Runs$N+All.Runs$Tie))*100, 1), "%"), 
            name="Losses",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#ff7979"))%>%
  layout(xaxis=list(title="Run Length",
                    dtick=1),
         yaxis=list(title="Result Rate",
                    dtick=.1,
                    tickformat ="0%"),
         title="Game Result Rate by Run Length",
         barmode = 'stack',
         paper_bgcolor="#f2f2f2",
         plot_bgcolor="#f2f2f2")

Best.Run<-PBP%>%select(gameID, teamName, Run, Winner)%>%
  filter(!is.na(Winner))%>%
  mutate(Val=1)%>%
  group_by(gameID, teamName)%>%
  mutate(MaxRun=max(Run))%>%select(-Run)%>%distinct()%>%
  ungroup()%>%
  select(MaxRun, Winner, Val)%>%
  pivot_wider(names_from = Winner, values_from = Val, values_fn = sum)%>%
  replace_na(list(Y=0, N=0, Tie=0))%>%
  mutate(Rate=Y/(Y+N+Tie))
  
plot_ly()%>%
  add_trace(type="bar", 
            x = Best.Run$MaxRun, 
            y=Best.Run$Y/(Best.Run$Y+Best.Run$N+Best.Run$Tie),  
            text=paste0(round((Best.Run$Y/(Best.Run$Y+Best.Run$N+Best.Run$Tie))*100, 1), "%"), 
            name="Victories",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#badc58"))%>%
  add_trace(type="bar", 
            x = Best.Run$MaxRun, 
            y=Best.Run$Tie/(Best.Run$Y+Best.Run$N+Best.Run$Tie), 
            text=paste0(round((Best.Run$Tie/(Best.Run$Y+Best.Run$N+Best.Run$Tie))*100, 1), "%"), 
            name="Ties",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#d0d0d0"))%>%
  add_trace(type="bar", 
            x = Best.Run$MaxRun, 
            y=Best.Run$N/(Best.Run$Y+Best.Run$N+Best.Run$Tie), 
            text=paste0(round((Best.Run$N/(Best.Run$Y+Best.Run$N+Best.Run$Tie))*100, 1), "%"), 
            name="Losses",
            marker=list(line=list(color="#000000", width=1.5),
                        color="#ff7979"))%>%
  layout(xaxis=list(title="Longest Run Length",
                    dtick=1),
         yaxis=list(title="Result Rate",
                    dtick=.1,
                    tickformat ="0%"),
         title="Game Result Rate by Longest Run Length",
         barmode = 'stack',
         paper_bgcolor="#f2f2f2",
         plot_bgcolor="#f2f2f2")


PBP%>%select(teamName,season, Run)%>%
  group_by(season, teamName)%>%
  summarise(AvgRun=mean(Run))%>%arrange(teamName, season)%>%View()
