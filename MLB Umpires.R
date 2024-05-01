library("tidyverse")
library("dplyr")

read.csv("games.csv") -> Games
read.csv("Independant Research Project - Umpires (Total).csv") -> Umpires
read.csv("Independant Research Project - Teams (Total).csv") -> Teams

Games$Fav_H <- as.numeric(Games$Fav_H)

# How many games would have had a different outcome with no favor
Games %>% select(R_H, R_A) -> Games_subset_2
Games_subset_2$RunDifferencial = Games_subset_2$R_H - Games_subset_2$R_A
Games %>% select(Year, Date, Umpire, Home, Away, R_H, R_A, Fav_H ) -> Games_subset
Games_subset$RunDifferencial = Games_subset_2$RunDifferencial
Games_subset$Fav_RD <- NA
Games_subset$Fav_RD <- ifelse(abs(Games_subset$Fav_H) > abs(Games_subset$RunDifferencial), 1, 0)
Games_subset$Ump_Overturn <- ifelse(abs(Games_subset$Fav_H) > abs(Games_subset$RunDifferencial) 
                                    & Games_subset$Fav_H > 0 
                                    & Games_subset$R_H > Games_subset$R_A
                                    | (abs(Games_subset$Fav_H) > abs(Games_subset$RunDifferencial))
                                    & Games_subset$Fav_H < 0
                                    & Games_subset$R_H < Games_subset$R_A, 1, 0)

# of Games the umpire could have changed the outcome
sum(Games_subset$Ump_Overturn == 1, na.rm = TRUE)

Overtuns_Per_Year <- Games_subset %>%
  group_by(Year) %>%
  count(Ump_Overturn, name = "Overturns") %>%
  filter(Ump_Overturn == 1)
Overtuns_Per_Year$Overturns <- as.numeric(Overtuns_Per_Year$Overturns)
write.csv(Overturns_Per_Year, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Overturns Per Year.csv")


# Group by Home & Away
Games_Subset_Teams <- Games_subset %>%
  group_by(Year, Home, Away, R_H, R_A, RunDifferencial, Ump_Overturn)
Games_Subset_Teams$Home_Overturn <- ifelse(Games_Subset_Teams$RunDifferencial >= 0
                                           & Games_Subset_Teams$Ump_Overturn == 1, 1, 0
                                           )
Games_Subset_Teams$Away_Overturn <- ifelse(Games_Subset_Teams$RunDifferencial <= 0
                                           & Games_Subset_Teams$Ump_Overturn == 1, 1, 0
                                           )
# Number of Home Overturns versus Away Overturns
sum(Games_Subset_Teams$Home_Overturn == 1, na.rm = TRUE)
sum(Games_Subset_Teams$Away_Overturn == 1, na.rm = TRUE)

# Group by Teams
Games_Subset_Teams_2 <- Games_Subset_Teams %>%
  group_by(Year, Home, Away) %>%
  mutate(Negative_overturn = ifelse("AZ" == Home & Home_Overturn == 1 | "AZ" == Away & Away_Overturn == 1, "AZ",
        ifelse("ATL" == Home & Home_Overturn == 1 | "ATL" == Away & Away_Overturn == 1, "ATL", 
        ifelse("BAL" == Home & Home_Overturn == 1 | "BAL" == Away & Away_Overturn == 1, "BAL",
        ifelse("BOS" == Home & Home_Overturn == 1 | "BOS" == Away & Away_Overturn == 1, "BOS",
        ifelse("BAL" == Home & Home_Overturn == 1 | "BAL" == Away & Away_Overturn == 1, "BAL",
        ifelse("CHC" == Home & Home_Overturn == 1 | "CHC" == Away & Away_Overturn == 1, "CHC",
        ifelse("CIN" == Home & Home_Overturn == 1 | "CIN" == Away & Away_Overturn == 1, "CIN",
        ifelse("CLE" == Home & Home_Overturn == 1 | "CLE" == Away & Away_Overturn == 1, "CLE",
        ifelse("COL" == Home & Home_Overturn == 1 | "COL" == Away & Away_Overturn == 1, "COL",
        ifelse("CWS" == Home & Home_Overturn == 1 | "CWS" == Away & Away_Overturn == 1, "CWS",
        ifelse("DET" == Home & Home_Overturn == 1 | "DET" == Away & Away_Overturn == 1, "DET",
        ifelse("HOU" == Home & Home_Overturn == 1 | "HOU" == Away & Away_Overturn == 1, "HOU",
        ifelse("KC" == Home & Home_Overturn == 1 | "KC" == Away & Away_Overturn == 1, "KC",
        ifelse("LAA" == Home & Home_Overturn == 1 | "LAA" == Away & Away_Overturn == 1, "LAA",
        ifelse("LAD" == Home & Home_Overturn == 1 | "LAD" == Away & Away_Overturn == 1, "LAD",
        ifelse("MIA" == Home & Home_Overturn == 1 | "MIA" == Away & Away_Overturn == 1, "MIA",
        ifelse("MIL" == Home & Home_Overturn == 1 | "MIL" == Away & Away_Overturn == 1, "MIL",
        ifelse("MIN" == Home & Home_Overturn == 1 | "MIN" == Away & Away_Overturn == 1, "MIN",
        ifelse("NYM" == Home & Home_Overturn == 1 | "NYM" == Away & Away_Overturn == 1, "NYM",
        ifelse("NYY" == Home & Home_Overturn == 1 | "NYY" == Away & Away_Overturn == 1, "NYY",
        ifelse("OAK" == Home & Home_Overturn == 1 | "OAK" == Away & Away_Overturn == 1, "OAK",
        ifelse("PHI" == Home & Home_Overturn == 1 | "PHI" == Away & Away_Overturn == 1, "PHI",
        ifelse("PIT" == Home & Home_Overturn == 1 | "PIT" == Away & Away_Overturn == 1, "PIT",
        ifelse("SD" == Home & Home_Overturn == 1 | "SD" == Away & Away_Overturn == 1, "SD",
        ifelse("SEA" == Home & Home_Overturn == 1 | "SEA" == Away & Away_Overturn == 1, "SEA",
        ifelse("SF" == Home & Home_Overturn == 1 | "SF" == Away & Away_Overturn == 1, "SF",
        ifelse("STL" == Home & Home_Overturn == 1 | "STL" == Away & Away_Overturn == 1, "STL",
        ifelse("TB" == Home & Home_Overturn == 1 | "TB" == Away & Away_Overturn == 1, "TB",
        ifelse("TEX" == Home & Home_Overturn == 1 | "TEX" == Away & Away_Overturn == 1, "TEX",
        ifelse("TOR" == Home & Home_Overturn == 1 | "TOR" == Away & Away_Overturn == 1, "TOR",
        ifelse("WSH" == Home & Home_Overturn == 1 | "WSH" == Away & Away_Overturn == 1, "WSH", "N/A" )))))))))))))))))))))))))))))))) %>%
      mutate(Positive_overturn = ifelse("AZ" == Home & Away_Overturn == 1 | "AZ" == Away & Home_Overturn == 1, "AZ",
        ifelse("ATL" == Home & Away_Overturn == 1 | "ATL" == Away & Home_Overturn == 1, "ATL", 
        ifelse("BAL" == Home & Away_Overturn == 1 | "BAL" == Away & Home_Overturn == 1, "BAL",
        ifelse("BOS" == Home & Away_Overturn == 1 | "BOS" == Away & Home_Overturn == 1, "BOS",
        ifelse("BAL" == Home & Away_Overturn == 1 | "BAL" == Away & Home_Overturn == 1, "BAL",
        ifelse("CHC" == Home & Away_Overturn == 1 | "CHC" == Away & Home_Overturn == 1, "CHC",
       ifelse("CIN" == Home & Away_Overturn == 1 | "CIN" == Away & Home_Overturn == 1, "CIN",
        ifelse("CLE" == Home & Away_Overturn == 1 | "CLE" == Away & Home_Overturn == 1, "CLE",
       ifelse("COL" == Home & Away_Overturn == 1 | "COL" == Away & Home_Overturn == 1, "COL",
        ifelse("CWS" == Home & Away_Overturn == 1 | "CWS" == Away & Home_Overturn == 1, "CWS",
       ifelse("DET" == Home & Away_Overturn == 1 | "DET" == Away & Home_Overturn == 1, "DET",
        ifelse("HOU" == Home & Away_Overturn == 1 | "HOU" == Away & Home_Overturn == 1, "HOU",
       ifelse("KC" == Home & Away_Overturn == 1 | "KC" == Away & Home_Overturn == 1, "KC",
        ifelse("LAA" == Home & Away_Overturn == 1 | "LAA" == Away & Home_Overturn == 1, "LAA",
       ifelse("LAD" == Home & Away_Overturn == 1 | "LAD" == Away & Home_Overturn == 1, "LAD",
        ifelse("MIA" == Home & Away_Overturn == 1 | "MIA" == Away & Home_Overturn == 1, "MIA",
       ifelse("MIL" == Home & Away_Overturn == 1 | "MIL" == Away & Home_Overturn == 1, "MIL",
        ifelse("MIN" == Home & Away_Overturn == 1 | "MIN" == Away & Home_Overturn == 1, "MIN",
       ifelse("NYM" == Home & Away_Overturn == 1 | "NYM" == Away & Home_Overturn == 1, "NYM",
        ifelse("NYY" == Home & Away_Overturn == 1 | "NYY" == Away & Home_Overturn == 1, "NYY",
       ifelse("OAK" == Home & Away_Overturn == 1 | "OAK" == Away & Home_Overturn == 1, "OAK",
        ifelse("PHI" == Home & Away_Overturn == 1 | "PHI" == Away & Home_Overturn == 1, "PHI",
       ifelse("PIT" == Home & Away_Overturn == 1 | "PIT" == Away & Home_Overturn == 1, "PIT",
        ifelse("SD" == Home & Away_Overturn == 1 | "SD" == Away & Home_Overturn == 1, "SD",
       ifelse("SEA" == Home & Away_Overturn == 1 | "SEA" == Away & Home_Overturn == 1, "SEA",
        ifelse("SF" == Home & Away_Overturn == 1 | "SF" == Away & Home_Overturn == 1, "SF",
       ifelse("STL" == Home & Away_Overturn == 1 | "STL" == Away & Home_Overturn == 1, "STL",
        ifelse("TB" == Home & Away_Overturn == 1 | "TB" == Away & Home_Overturn == 1, "TB",
       ifelse("TEX" == Home & Away_Overturn == 1 | "TEX" == Away & Home_Overturn == 1, "TEX",
        ifelse("TOR" == Home & Away_Overturn == 1 | "TOR" == Away & Home_Overturn == 1, "TOR",
ifelse("WSH" == Home & Away_Overturn == 1 | "WSH" == Away & Home_Overturn == 1, "WSH", "N/A" ))))))))))))))))))))))))))))))))

# Sorting # of overturns per team
Wins_Lost <- Games_Subset_Teams_2 %>%
  group_by(Home, Year, Negative_overturn) %>%
  mutate(Count_Wins_Lost_PerYear = ifelse(Home == Negative_overturn, 1, ifelse(Away == Negative_overturn, 1, 0)))
Wins_Lost_2 <- Wins_Lost %>%
  group_by(Negative_overturn, Year) %>%
  count(Count_Wins_Lost_PerYear, name = "Wins_Lost")
Potential_Wins_Lost <- Wins_Lost_2[Wins_Lost_2$Count_Wins_Lost_PerYear == 1,]
Wins_Lost_Team <- Wins_Lost %>%
  group_by(Negative_overturn) %>%
  count(Count_Wins_Lost_PerYear, name = "Wins_Lost") %>%
  filter(Count_Wins_Lost_PerYear == 1)
Wins_Gained <- Games_Subset_Teams_2 %>%
  group_by(Home, Year, Positive_overturn) %>%
  mutate(Count_Wins_Gained_PerYear = ifelse(Home == Positive_overturn, 1, ifelse(Away == Positive_overturn, 1, 0)))
Wins_Gained_2 <- Wins_Gained %>%
  group_by(Positive_overturn, Year) %>%
  count(Count_Wins_Gained_PerYear, name = "Wins_Gained")
Potential_Wins_Gained <-  Wins_Gained_2[Wins_Gained_2$Count_Wins_Gained_PerYear == 1,]
Wins_Gained_Team <- Wins_Gained %>%
  group_by(Positive_overturn) %>%
  count(Count_Wins_Gained_PerYear, name = "Wins_Gained") %>%
  filter(Count_Wins_Gained_PerYear == 1)

sum(Potential_Wins_Gained$Wins_Gained, na.rm = TRUE)
sum(Potential_Wins_Lost$Wins_Lost, na.rm = TRUE)

colnames(Wins_Gained_Team) <- c("Team", "Count", "Wins_Gained")
colnames(Wins_Lost_Team) <- c("Team", "Count", "Wins_Lost")

colnames(Potential_Wins_Gained) <- c("Team", "Year", "Count", "Wins_Gained")
colnames(Potential_Wins_Lost) <- c("Team", "Year", "Count", "Wins_Lost")

Team_Wins_Losses <- Wins_Gained_Team %>%
  full_join(Wins_Lost_Team, by=c("Team" = "Team")) %>% 
  select (-2, -4) %>%
  mutate(Team_PlusMinus = (Wins_Gained - Wins_Lost))

Team_Wins_Losses_Annually <- Potential_Wins_Gained %>%
  full_join(Potential_Wins_Lost, by=c("Team" = "Team", "Year" = "Year")) %>%
  select (-3, -5) %>%
  mutate(Team_PlusMinus = (Wins_Gained - Wins_Lost))

sum(Team_Wins_Losses$Wins_Gained)
sum(Team_Wins_Losses$Wins_Lost)
sum(Team_Wins_Losses$Team_PlusMinus)

write.csv(Team_Wins_Losses, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Team_Wins_Losses.csv")
write.csv(Team_Wins_Losses_Annually, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Team_Wins_Losses_Annually.csv")

# Extra Inning Win Probability
Games_subset_Sim <- Games_subset %>%
  filter(Ump_Overturn == 1) 
Games_subset_Sim_2 <- Games_subset_Sim %>%
  mutate(Run_Differencial_Fav =  ifelse(Games_subset_Sim$Fav_H >= 1 & Games_subset_Sim$RunDifferencial == 1, (Games_subset_Sim$Fav_H - 1), 
                                 ifelse(Games_subset_Sim$Fav_H <= -1 & Games_subset_Sim$RunDifferencial == -1, (Games_subset_Sim$Fav_H +1),
                                 ifelse(Games_subset_Sim$Fav_H >= 2 & Games_subset_Sim$RunDifferencial == 2, (Games_subset_Sim$Fav_H - 2),
                                 ifelse(Games_subset_Sim$Fav_H <= -2 & Games_subset_Sim$RunDifferencial == -2, (Games_subset_Sim$Fav_H +2),
                                 ifelse(Games_subset_Sim$Fav_H >= 3 & Games_subset_Sim$RunDifferencial == 3, (Games_subset_Sim$Fav_H - 3),
                                 ifelse(Games_subset_Sim$Fav_H <= -3 & Games_subset_Sim$RunDifferencial == -3, (Games_subset_Sim$Fav_H +3),
                                 0))))))) %>%
  mutate(R_H_Fav = ifelse(Games_subset_Sim$Fav_H >= 0 & (Games_subset_Sim$R_H - abs(Games_subset_Sim$Fav_H)) >= 0,
                                 (Games_subset_Sim$R_H - abs(Games_subset_Sim$Fav_H)), 
                                 ifelse(Games_subset_Sim$Fav_H >= 0 & (Games_subset_Sim$R_H - abs(Games_subset_Sim$Fav_H)) <= 0, 0, 
                                 Games_subset_Sim$R_H))) %>%
  mutate(R_A_Fav = ifelse(Games_subset_Sim$Fav_H <= 0 & (Games_subset_Sim$R_A - abs(Games_subset_Sim$Fav_H)) >= 0,
                                 (Games_subset_Sim$R_A - abs(Games_subset_Sim$Fav_H)),
                                 ifelse(Games_subset_Sim$Fav_H <= 0 & (Games_subset_Sim$R_A - abs(Games_subset_Sim$Fav_H)) <= 0, 0,
                                 Games_subset_Sim$R_A))) %>%
  mutate(Extra_Innings_WP = ifelse(Run_Differencial_Fav >= 1, 0,
                            ifelse(Run_Differencial_Fav <= -1, 1,
                            ifelse(Games_subset_Sim_2$R_A_Fav == 0 & Games_subset_Sim_2$R_H_Fav == 0, .5,
                            ((Games_subset_Sim_2$R_H_Fav ^ 2)/((Games_subset_Sim_2$R_H_Fav ^ 2) + (Games_subset_Sim_2$R_A_Fav ^ 2)))))))
#Merging Files

Teams_Plus_Sim = merge(Games_Subset_Teams_2, Games_subset_Sim_2, by=c('Year','Date','Umpire','Home','Away','RunDifferencial','Fav_RD', 'R_H', 'R_A', 'Fav_H', 'Ump_Overturn'), all = TRUE)
write_csv(Teams_Plus_Sim, "Team_Plus_Sim.csv")
Overturns_Plus_EIWP <- Teams_Plus_Sim %>%
  filter(Ump_Overturn == 1)
write.csv(Overturns_Plus_EIWP, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Overturns_Plus_EIWP.csv", row.names=FALSE)
Team_Plus_Minus_Anually <- read.csv("Team_Wins_Losses_Annually.csv")


Teams_Plus_Overturns <- merge(Teams, Team_Plus_Minus_Anually, by=c('Team', 'Year'), all = TRUE)


write.csv(Teams_Plus_Overturns, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Teams_Plus_Overturns.csv", row.names=FALSE)

# Group by Umpires
Games_subset_Umpire <- Games_subset %>%
  group_by(Umpire, Year, Ump_Overturn) %>%
  count(Ump_Overturn)
View(Games_subset_Umpire)

bias <- Games_subset %>%
  group_by(Home, Umpire) %>%
  summarise(bias_check = mean(Fav_H))

bias_2 <- Games_subset %>%
  group_by(Home, Umpire) %>%
  count()

bais_total <- bias %>%
  left_join(bias_2, by = c("Home" = "Home", "Umpire" = "Umpire"))

mean(bais_total$bias_check)
mean(bais_total$n)

sd(bais_total$bias_check)
sd(bais_total$n)

bias_check_2 <- bais_total %>%
  mutate(possible_bias = ifelse(n >= 5.355014+2.934036 & bias_check >= 0.02933981+(2*0.3547371) | n >= 5.355014+2.934036 & bias_check <= 0.02933981+(2*0.3547371), 1, 0))

Games_subset_Umpire_2 <- Games_subset_Umpire
  Games_subset_Umpire_2$Umpire_Overturns <- NA
  Games_subset_Umpire_2$Umpire_Overturns <- ifelse(Games_subset_Umpire$Ump_Overturn > 0, print(num(Games_subset_Umpire$n)), print(0))
View(Games_subset_Umpire_2)  
  
write.csv(Games_subset_Umpire_2, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Games_subset_Umpire_2.csv", row.names=FALSE)
read.csv("Games_subset_Umpire_Update.csv") -> Games_subset_Umpire_Update
view(Games_subset_Umpire_Update)

Games_subset_Umpire_Update %>% select(Umpire, Year, Umpire_Overturns) -> Games_subset_Umpire_Merge

Umpire_Profiling <- read.csv("Independant Research Project - Umpires (Profiling).csv")

Umpire_Merged <- merge(Umpire_Profiling,Games_subset_Umpire_Merge,by=c('Umpire','Year'),all.x=T)

write.csv(Umpire_Merged, "C:\\Users\\jman2\\OneDrive\\Documents\\R\\RData\\MLB Umpires\\Umpires_Plus_UmpOverturns.csv", row.names=FALSE)
Umpires_Plus_UmpOverturns <- read.csv("Umpires_Plus_UmpOverturns.csv")
view(Umpires_Plus_UmpOverturns)

## Extra Innings Win Probability
view(Games_subset)
Games_subset_Sim <- Games_subset %>%
  filter(Ump_Overturn == 1) 
Games_subset_Sim_2 <- Games_subset_Sim %>%
  mutate(Run_Differencial_Fav =  ifelse(Games_subset_Sim$Fav_H >= 1 & Games_subset_Sim$RunDifferencial == 1, (Games_subset_Sim$Fav_H - 1), 
                    ifelse(Games_subset_Sim$Fav_H <= -1 & Games_subset_Sim$RunDifferencial == -1, (Games_subset_Sim$Fav_H +1),
                    ifelse(Games_subset_Sim$Fav_H >= 2 & Games_subset_Sim$RunDifferencial == 2, (Games_subset_Sim$Fav_H - 2),
                    ifelse(Games_subset_Sim$Fav_H <= -2 & Games_subset_Sim$RunDifferencial == -2, (Games_subset_Sim$Fav_H +2),
                    ifelse(Games_subset_Sim$Fav_H >= 3 & Games_subset_Sim$RunDifferencial == 3, (Games_subset_Sim$Fav_H - 3),
                    ifelse(Games_subset_Sim$Fav_H <= -3 & Games_subset_Sim$RunDifferencial == -3, (Games_subset_Sim$Fav_H +3),
                           0))))))) %>%
  mutate(R_H_Fav = ifelse(Games_subset_Sim_2$Fav_H >= 0 & (Games_subset_Sim_2$R_H - abs(Games_subset_Sim_2$Fav_H)) >= 0,
                                                          (Games_subset_Sim_2$R_H - abs(Games_subset_Sim_2$Fav_H)), 
                   ifelse(Games_subset_Sim_2$Fav_H >= 0 & (Games_subset_Sim_2$R_H - abs(Games_subset_Sim_2$Fav_H)) <= 0, 0, 
                                                           Games_subset_Sim_2$R_H))) %>%
  mutate(R_A_Fav = ifelse(Games_subset_Sim_2$Fav_H <= 0 & (Games_subset_Sim_2$R_A - abs(Games_subset_Sim_2$Fav_H)) >= 0,
                                                          (Games_subset_Sim_2$R_A - abs(Games_subset_Sim_2$Fav_H)),
                   ifelse(Games_subset_Sim_2$Fav_H <= 0 & (Games_subset_Sim_2$R_A - abs(Games_subset_Sim_2$Fav_H)) <= 0, 0,
                                                           Games_subset_Sim_2$R_A))) %>%
  mutate(Extra_Innings_WP = ifelse(Run_Differencial_Fav >= 1, 0,
                            ifelse(Run_Differencial_Fav <= -1, 1,
                            ifelse(Games_subset_Sim_2$R_A_Fav == 0 & Games_subset_Sim_2$R_H_Fav == 0, .5,
                            ((Games_subset_Sim_2$R_H_Fav ^ 2)/((Games_subset_Sim_2$R_H_Fav ^ 2) + (Games_subset_Sim_2$R_A_Fav ^ 2)))))))
 
##Home runs + Favor / Total runs + favor --> play around with probability to see outcome
##Get expected runs by home or away team in extra innings find variance and equate favor to see which team would score more runs

Extra_Innings_sim <- function(reps) {
  possible_winnings <- c(-1, 1)
  probabilities <- c(Games_subset_Sim$Sim_Fav * Games_subset_Sim$Extra_Innings_WP)
  winnings <- numeric(length = reps)
  for (i in 1:reps) {
    winnings[i] <- sample(
      possible_winnings,
      size = 1,
      prob = probabilities
    )
  }
  mean(winnings)
}


coin_game_sim <- function(reps) {
  ## set up the game:
  possible_winnings <- c(-1, 0, 2)
  probabilities <- c(0.25, 0.50, 0.25)
  ## set up the results vector:
  winnings <- numeric(length = reps)
  ## one-by-one, get the results:
  for (i in 1:reps) {
    winnings[i] <- sample(
      possible_winnings,
      size = 1,
      prob = probabilities
    )
  }
  ## return the estimate of expected value:
  mean(winnings)
}